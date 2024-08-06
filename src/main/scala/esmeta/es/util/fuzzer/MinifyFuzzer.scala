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

  val minifyTester = MinifyTester(
    cfg,
    MinifyTesterConfig(
      timeLimit = timeLimit,
    ),
  )

  val tracerInjector = TracerInjector(using cfg)

  lazy val result: Coverage = fuzzer.result

  lazy val db: MinifierDB = MinifierDB.fromResource

  val filteredAOs: List[String] = List(
    "INTRINSICS.Function.prototype.toString",
  )
  val ignoreProperties: List[String] = List("name").map(prop => s"\"$prop\"")

  // delta -> unique bug counter in this iteration
  var minimalIterMap: MMap[String, Int] = MMap.empty

  // delta -> original programs
  val minimalMap: MMap[String, MSet[String]] = MMap.empty

  // set of programs we already saw
  def pass: Set[String] = (db.minimals ++ minimalIterMap.keys).toSet

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
    // TODO(@hyp3rflow): we should consider about same iter number among different programs due to return injector
    iter: Int,
    finalState: State,
    code: String,
    covered: Boolean,
  ): Unit =
    val injector = ReturnInjector(cfg, finalState, timeLimit, false)
    injector.exitTag match
      case NormalTag =>
        val returns = injector.assertions
        // TODO: some simple programs cannot be checked by this logic due to the empty return assertion
        for (ret <- returns.par) {
          // TODO: we have to try in sloppy mode but ESMeta doesn't respect execution mode yet
          val directive = USE_STRICT
          val fn = s"function () {\n$code\n$ret\n}"
          val iife = s"const k = ($fn)();\n"
          val instrumentedFn = tracerInjector(fn)
          val instrumentedIife = s"const k = ($instrumentedFn)();\n"
          val original = directive ++ iife
          val tracerHeader =
            "const arr = []; const tracer = i => arr.push(i);\n"
          val tracedOriginal = directive ++ tracerHeader ++ instrumentedIife
          for (code <- List(original, tracedOriginal))
            dumpFile(code, s"$logDir/tmp.js")
            minifyTester.test(code) match
              case None =>
              case Some(
                    MinifyTesterResult(code, minified, injected, exception),
                  ) =>
                val delta =
                  DeltaDebugger(
                    cfg, {
                      minifyTester.test(_) match
                        case Some(result) => exception == result.exception
                        case _            => false
                    },
                  ).result(code)
                log(
                  MinifyFuzzResult(
                    iter,
                    covered,
                    code,
                    minified,
                    delta,
                    injected,
                    exception,
                  ),
                )
        }
      case _ =>

  private def log(result: MinifyFuzzResult) = minimalIterMap.synchronized {
    val MinifyFuzzResult(
      iter,
      covered,
      original,
      minified,
      delta,
      injected,
      exception,
    ) = result
    // if it is new, we have to log
    if (!pass.contains(delta)) {
      val count = counter.incrementAndGet()
      minimalIterMap += (delta -> count)
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
      // TODO(@hyp3rflow): extends Fuzzer and dumps DB periodically.
      // dumpJson(db.asJson, s"$dirpath/db.json")
    }
    // we have to log original program even if it is not new
    minimalIterMap.get(delta) match
      case Some(count) =>
        val dirpath = s"$logDir/$count/bugs"
        mkdir(dirpath)
        dumpFile(original, s"$dirpath/$iter.js")
        minimalMap.getOrElseUpdate(delta, MSet.empty).add(original)
      case None if db.getLabel(delta).isDefined =>
        val label = db.getLabel(delta).get
        val dirpath = s"$logDir/labels/$label"
        mkdir(dirpath)
        dumpFile(original, s"$dirpath/$iter.js")
        dumpFile(exception, s"$dirpath/$iter.reason.txt")
      // do not add minimalMap to keep memory size small
      case _ =>
        error("Unexpected program in log")

  }

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
