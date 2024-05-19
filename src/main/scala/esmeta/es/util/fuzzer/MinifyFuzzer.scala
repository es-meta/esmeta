package esmeta.es.util.fuzzer

import esmeta.util.*
import esmeta.cfg.CFG
import esmeta.{error => _, *}
import esmeta.es.util.Coverage
import esmeta.es.util.fuzzer.Fuzzer.NO_DEBUG
import esmeta.injector.*
import esmeta.state.State
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import esmeta.js.JSEngine
import esmeta.js.minifier.Minifier
import esmeta.es.util.USE_STRICT
import esmeta.es.util.delta.DeltaDebugger
import scala.util.*
import scala.collection.parallel.CollectionConverters._
import scala.collection.mutable.{Map => MMap, Set => MSet}

import io.circe.Json
import java.util.concurrent.atomic.AtomicInteger

object MinifyFuzzer {
  def apply(
    cfg: CFG,
    logInterval: Option[Int] = Some(600), // default is 10 minutes.
    debug: Int = NO_DEBUG, // 2: all, 1: partial, 0: no
    stdOut: Boolean = false,
    timeLimit: Option[Int] = None, // time limitation for each evaluation
    trial: Option[Int] = None, // `None` denotes no bound
    duration: Option[Int] = None, // `None` denotes no bound
    init: Option[String] = None,
    kFs: Int = 0,
    cp: Boolean = false,
  ): Coverage = new MinifyFuzzer(
    cfg,
    logInterval,
    debug,
    stdOut,
    timeLimit,
    trial,
    duration,
    init,
    kFs,
    cp,
  ).result

  val logDir: String = s"$MINIFY_FUZZ_LOG_DIR/fuzz-$dateStr"
  val symlink: String = s"$MINIFY_FUZZ_LOG_DIR/recent"
}

class MinifyFuzzer(
  cfg: CFG,
  logInterval: Option[Int] = Some(600), // default is 10 minutes.
  debug: Int = NO_DEBUG, // 2: all, 1: partial, 0: no
  stdOut: Boolean = false,
  timeLimit: Option[Int] = None, // time limitation for each evaluation
  trial: Option[Int] = None, // `None` denotes no bound
  duration: Option[Int] = None, // `None` denotes no bound
  init: Option[String] = None,
  kFs: Int = 0,
  cp: Boolean = false,
) {
  import MinifyFuzzer.*

  val counter = AtomicInteger(0)

  lazy val result: Coverage =
    Fuzzer(
      cfg = cfg,
      logInterval = logInterval,
      debug = debug,
      timeLimit = timeLimit,
      trial = trial,
      duration = duration,
      init = init,
      kFs = kFs,
      cp = cp,
    ).result

  val filteredAOs: List[String] = List(
    "INTRINSICS.Function.prototype.toString",
  )
  val ignoreProperties: List[String] = List("name").map(prop => s"\"$prop\"")

  var minimals: MMap[String, Int] = MMap.empty
  val minimalMap: MMap[String, MSet[String]] = MMap.empty

  lazy val fuzzer = new Fuzzer(
    cfg = cfg,
    logInterval = logInterval,
    debug = debug,
    timeLimit = timeLimit,
    trial = trial,
    duration = duration,
    kFs = kFs,
    cp = cp,
    init = init,
  ) {
    override lazy val logDir = MinifyFuzzer.logDir
    override lazy val symlink = MinifyFuzzer.symlink
    override def add(code: String, info: CandInfo): Boolean = handleResult(
      Try {
        if (info.visited)
          fail("ALREADY VISITED")
        visited += code
        if (info.invalid)
          fail("INVALID PROGRAM")
        val script = toScript(code)
        val interp = info.interp.getOrElse(fail("Interp Fail"))
        val finalState = interp.result
        val (_, updated, covered) = cov.check(script, interp)
        val filtered = interp.coveredAOs intersect filteredAOs
        if (filtered.isEmpty)
          minifyTest(iter, finalState, code, covered)
        else println(s"PASS minifier check due to: $filtered")
        if (!updated) fail("NO UPDATE")
        covered
      },
    )
  }

  private def minifyTest(
    iter: Int,
    finalState: State,
    code: String,
    covered: Boolean,
  ) =
    val injector = ReturnInjector(cfg, finalState, timeLimit, false)
    injector.exitTag match
      case NormalTag =>
        val returns = injector.assertions
        // TODO: some simple programs cannot be checked by this logic due to the empty return assertion
        for (ret <- returns.par) {
          val original =
            s"${USE_STRICT}const k = (() => {\n$code\n$ret\n})();\n"
          // get assertions from original code to compare with delta-debugged code
          val assertions = Injector
            .getTest(
              cfg,
              original,
              timeLimit = timeLimit,
              ignoreProperties = ignoreProperties,
            )
            .assertions
          Minifier.minifySwc(original) match
            case Failure(exception) => println(s"[minify-fuzz] $exception")
            case Success(minified) => {
              val injected =
                Injector.replaceBody(
                  cfg,
                  original,
                  minified,
                  defs = true,
                  timeLimit = timeLimit,
                  ignoreProperties = ignoreProperties,
                )
              injected.exitTag match
                case NormalTag =>
                  val injectedCode = injected.toString
                  JSEngine.runGraal(injectedCode, Some(1000)) match
                    // minified program passes assertions
                    case Success(v) if v.isEmpty =>
                      println(s"[minify-fuzz] pass")
                    // minified program fails on assertions
                    case Success(v) =>
                      println(s"[minify-fuzz] return value exists")
                      val delta = deltaDebug(
                        original,
                        injected.exitTag,
                        assertions,
                        { code =>
                          JSEngine.runGraal(code, Some(1000)) match
                            case Success(mv) if !mv.isEmpty => mv == v
                            case _                          => false
                        },
                      )(original)
                      log(
                        MinifyFuzzResult(
                          iter,
                          covered,
                          original,
                          minified,
                          delta,
                          injectedCode,
                          v,
                        ),
                      )
                    // minified program throws exception
                    // TODO(@hyp3rflow): we have to mask span data of program exception
                    case Failure(exception) =>
                      println(s"[minify-fuzz] exception throws")
                      val delta = deltaDebug(
                        original,
                        injected.exitTag,
                        assertions,
                        { code =>
                          JSEngine.runGraal(code, Some(1000)) match
                            case Failure(e) => e.toString == exception.toString
                            case _          => false
                        },
                      )(original)
                      log(
                        MinifyFuzzResult(
                          iter,
                          covered,
                          original,
                          minified,
                          delta,
                          injectedCode,
                          exception.toString,
                        ),
                      )
                case _ => println("[minify-fuzz] exit state is not normal")
            }
        }
      case _ =>

  private def log(result: MinifyFuzzResult) = minimals.synchronized {
    val MinifyFuzzResult(
      iter,
      covered,
      original,
      minified,
      delta,
      injected,
      exception,
    ) = result
    if (!minimals.contains(delta)) {
      val count = counter.incrementAndGet()
      minimals += (delta -> count)
      val dirpath = s"$logDir/$count"
      mkdir(dirpath)
      dumpFile(original, s"$dirpath/original.js")
      dumpFile(minified, s"$dirpath/minified.js")
      dumpFile(injected, s"$dirpath/injected.js")
      dumpFile(delta, s"$dirpath/delta.js")
      dumpFile(exception, s"$dirpath/reason")
      dumpJson(
        Json.obj(
          "iter" -> Json.fromInt(iter),
          "covered" -> Json.fromBoolean(covered),
        ),
        s"$dirpath/info",
      )
    }
    val count = minimals.get(delta)
    val dirpath = s"$logDir/$count/bugs"
    mkdir(dirpath)
    dumpFile(original, s"$dirpath/$iter.js")
    minimalMap.getOrElseUpdate(delta, MSet.empty).add(original)
  }

  private def deltaDebug(
    originalCode: String,
    exitTag: ExitTag,
    assertions: Vector[Assertion],
    checker: (delta: String) => Boolean,
  ) =
    val debugger = DeltaDebugger(
      cfg,
      delta => {
        Minifier.minifySwc(delta) match
          case Failure(_)        => false
          case Success(minified) =>
            // check delta is valid
            val deltaAssertions =
              Injector
                .getTest(
                  cfg,
                  delta,
                  timeLimit = timeLimit,
                  ignoreProperties = ignoreProperties,
                )
                .assertions
            val injected = Injector.replaceBody(
              cfg,
              originalCode,
              minified,
              defs = true,
              timeLimit = timeLimit,
              ignoreProperties = ignoreProperties,
            )
            injected.exitTag match
              case tag if exitTag == tag =>
                if (assertions != deltaAssertions) false
                else checker(injected.toString)
              case _ => false
      },
    )
    debugger.result
}

case class MinifyFuzzResult(
  iteration: Int,
  covered: Boolean,
  originalCode: String,
  minifiedCode: String,
  deltaDebugged: String,
  injectedCode: String,
  exception: String,
)
