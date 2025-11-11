package esmeta.phase

import esmeta.{CommandConfig, TEST_MODE}
import esmeta.cfg.CFG
import esmeta.error.*
import esmeta.interpreter.*
import esmeta.state.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.{ConcurrentPolicy => CP}
import esmeta.util.SystemUtils.*
import esmeta.es.*
import esmeta.es.util.Coverage
import esmeta.test262.{*, given}
import esmeta.test262.jalangi.*
import esmeta.test262.util.{TestFilter, Summary}
import java.io.File
import java.util.concurrent.TimeoutException

/** `jalangi-test` phase */
case object JalangiTest extends Phase[CFG, Unit] {
  val name = "jalangi-test"
  val help = "tests Test262 tests with harness files (default: tests/test262)."
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Unit =
    // set test mode
    TEST_MODE = true

    // get target version of Test262
    val version = Test262.getVersion(config.target)
    val test262 = Test262(version, cfg, config.withYet)
    val targets =
      if (cmdConfig.targets.isEmpty) None
      else Some(cmdConfig.targets)

    if (config.timeLimit.isDefined && config.concurrent == CP.Auto)
      raise(
        "Turing on both time limit option (-test262-test:timeout and " +
        "the concurrent mode (-test262-test:concurrent) with " +
        "automatic thread number is not allowed.",
      )

    // run test262 eval test
    val summary = Jalangi.test(targets, timeLimit = Some(60000))(using test262)

    println(summary)

  // if summary has failed test case, throws an exception
  // if (summary.failCount > 0) throw Test262Fail(summary.fail)

  // return summary
  // summary

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "target",
      StrOption((c, s) => c.target = Some(s)),
      "set the target git version of Test262 (default: current version).",
    ),
    (
      "progress",
      BoolOption(_.progress = _),
      "show progress bar.",
    ),
    (
      "timeout",
      NumOption((c, k) => c.timeLimit = Some(k)),
      "set the time limit in seconds (default: no limit).",
    ),
    (
      "with-yet",
      BoolOption(_.withYet = _),
      "test with currently ignored tests because of unknown issues.",
    ),
    (
      "log",
      BoolOption(_.log = _),
      "turn on logging mode.",
    ),
    (
      "detail-log",
      BoolOption((c, b) => { c.log ||= b; c.detail = b }),
      "turn on logging mode with detailed information.",
    ),
    (
      "concurrent",
      NumOption((c, k) =>
        c.concurrent =
          if (k <= 0) then CP.Auto else if (k == 1) CP.Single else CP.Fixed(k),
      ),
      "set the number of thread to use concurrently (default: no concurrent)." +
      " If number <= 0, use automatically determined number of threads.",
    ),
  )
  case class Config(
    var target: Option[String] = None,
    var progress: Boolean = false,
    var timeLimit: Option[Int] = None,
    var withYet: Boolean = false,
    var log: Boolean = false,
    var detail: Boolean = false,
    var concurrent: CP = CP.Single,
  )
}
