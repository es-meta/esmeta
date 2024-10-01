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
import esmeta.mutator.TracerExprMutator

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
    dumpDetail: Int = 2, // 2: all, 1: partial, 0: no
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
    dumpDetail,
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
  dumpDetail: Int = 2, // 2: all, 1: partial, 0: no
) {
  import MinifyFuzzer.*

  val bugIndexCounter = AtomicInteger(0)

  val minifyTester = MinifyTester(
    cfg,
    MinifyTesterConfig(
      timeLimit = timeLimit,
      ignoreProperties = List("name").map(prop => s"\"$prop\""),
      debugLevel = debug,
    ),
  )

  val tracerExprMutator = TracerExprMutator(using cfg)
  val tracerInjector = TracerInjector(using cfg)

  lazy val result: Coverage = fuzzer.result

  lazy val db: MinifierDB = MinifierDB.fromResource

  val filteredAOs: List[String] = List(
    "INTRINSICS.Function.prototype.toString",
  )

  // delta -> unique bug index
  val deltaIndex: MMap[String, Int] = MMap.empty

  // delta -> original programs
  val deltaProvenances: MMap[String, MSet[String]] = MMap.empty

  // set of already seen programs (deltas)
  def pass: Set[String] = (db.minimals ++ deltaIndex.keys).toSet

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

    // adjust weight for active random fuzzing
    override val selector: TargetSelector = WeightedSelector(
      RandomSelector -> 8,
      BranchSelector -> 2,
    )

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

  private def buildTestProgram(code: String, ret: ReturnAssertion): String =
    val instrumentedCode = tracerInjector(code)
    val iife = s"const k = (function () {\n$code\n$ret\n})();\n"
    val tracerHeader =
      s"const arr = []; const $TRACER_SYMBOL = x => (arr.push(x), x)\n"
    USE_STRICT ++ tracerHeader ++ iife

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
        val codes =
          // TODO: we have to try in sloppy mode but ESMeta doesn't respect execution mode yet
          code +: tracerExprMutator(code, 5, None).map(
            _._2.toString(grammar = Some(cfg.grammar)),
          )
        for {
          ret <- returns.par
          code <- codes.par
        } {
          val original = buildTestProgram(code, ret)
          minifyTester.test(original) match
            case None | Some(_: AssertionSuccess) =>
            case Some(failure) =>
              val delta =
                DeltaDebugger(
                  cfg,
                  code =>
                    minifyTester
                      .test(buildTestProgram(code, ret))
                      .fold(false)(_.tag == failure.tag),
                ).result(code)
              // re-run tester with dd output
              minifyTester.test(buildTestProgram(delta, ret)) match
                case None | Some(_: AssertionSuccess) =>
                case Some(result) =>
                  log(MinifyFuzzResult(iter, covered, original, result))
        }

      case _ =>

  private def log(result: MinifyFuzzResult) = deltaIndex.synchronized {
    val MinifyFuzzResult(iter, covered, original, test) = result
    val delta = test.original
    val minified = test.minified
    val injected = test.injected
    // if it is new, we have to log
    if (!pass.contains(delta)) {
      val count = bugIndexCounter.incrementAndGet()
      deltaIndex += (original -> count)
      val dirpath = s"$logDir/$count"
      mkdir(dirpath)
      dumpFile(minified, s"$dirpath/minified.js")
      dumpFile(injected, s"$dirpath/injected.js")
      dumpFile(delta, s"$dirpath/delta.js")
      test.getReason.map(dumpFile(_, s"$dirpath/reason"))
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
    deltaIndex.get(delta) match
      // if it is found in this execution, dump original to bug index directory.
      case Some(index) =>
        val dirpath = s"$logDir/$index/bugs"
        mkdir(dirpath)
        dumpFile(original, s"$dirpath/$iter.js")
        deltaProvenances.getOrElseUpdate(delta, MSet.empty).add(original)
      // if it is already known bug, dump original to label directory.
      case None if db.getLabel(delta).isDefined =>
        val label = db.getLabel(delta).get
        val dirpath = s"$logDir/labels/$label"
        mkdir(dirpath)
        dumpFile(original, s"$dirpath/$iter.js")
        test.getReason.map(dumpFile(_, s"$dirpath/$iter.reason.txt"))
      // unreachable path
      case _ => ???

  }

}

case class MinifyFuzzResult(
  iteration: Int,
  covered: Boolean,
  original: String,
  result: MinifyTestResult,
)
