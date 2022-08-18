package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.error.*
import esmeta.interpreter.*
import esmeta.state.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import esmeta.es.*
import esmeta.es.util.Coverage
import esmeta.test262.{*, given}
import esmeta.test262.util.TestFilter
import java.io.File
import java.util.concurrent.TimeoutException

/** `test262-test` phase */
case object Test262Test extends Phase[CFG, Summary] {
  val name = "test262-test"
  val help = "test Test262 tests with harness files (default: tests/test262)."
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Summary =
    // get specification
    val spec = cfg.spec

    // get metadata list
    val dataList: List[MetaData] = (for {
      path <- cmdConfig.args match
        case Nil   => List(TEST262_TEST_DIR)
        case paths => paths
      data <- MetaData.fromDir(path)
    } yield data).sorted

    // multiple targets
    val multiple = dataList.length > 1

    // get all applicable tests
    val tests = ProgressBar[NormalConfig](
      msg = "Run Test262 tests",
      iterable = TestFilter(dataList).summary.normal,
      getName = (config, _) => config.name,
      errorHandler = (e, summary, name) => {
        if (multiple) e match
          case NotSupported(msg)   => summary.yets += s"$name - $msg"
          case _: TimeoutException => summary.timeouts += name
          case e: Throwable => summary.fails += s"$name - ${e.getMessage}"
        else throw e
      },
      verbose = config.progress,
    )

    // test progress summary
    val summary = tests.summary

    // get target version of Test262
    val version = Test262.getVersion(config.target)
    val test262 = Test262(version, spec)

    // coverage with time limit
    lazy val cov = Coverage(
      cfg = cfg,
      test262 = Some(test262),
      timeLimit = config.timeLimit,
    )

    // logging mode
    val logDir = s"$TEST262TEST_LOG_DIR/$dateStr"
    if (config.log && multiple)
      println(s"- Logging to $logDir...")
      mkdir(logDir)
      val ecma262Version = spec.version.fold("<none>")(_.toString)
      dumpFile(ecma262Version, s"$logDir/ecma262-version")
      dumpFile(ESMeta.currentVersion, s"$logDir/esmeta-version")
      summary.timeouts.setPath(s"$logDir/eval-timeout.log")
      summary.yets.setPath(s"$logDir/eval-yet.log")
      summary.fails.setPath(s"$logDir/eval-fail.log")
      summary.passes.setPath(s"$logDir/eval-pass.log")

    // run tests
    for (test <- tests)
      val NormalConfig(filename, includes) = test
      val st = if (!config.coverage) {
        val script = test262.loadTest(filename)
        val code = script.toString(grammar = Some(cfg.grammar)).trim
        val st = Initialize(cfg, code, Some(script))
        Interpreter(
          st = st,
          log = !multiple && config.log,
          logDir = TEST262TEST_LOG_DIR,
          timeLimit = config.timeLimit,
        )
      } else cov.run(filename)
      val returnValue = st(GLOBAL_RESULT)
      if (returnValue != Undef) throw InvalidExit(returnValue)

    // run tests
    if (config.log && multiple)
      summary.close
      val summaryStr =
        if (config.coverage) s"$summary$LINE_SEP$cov"
        else s"$summary"
      dumpFile("Test262 test summary", summaryStr, s"$logDir/eval-summary")

    // dump coverage
    if (config.coverage) cov.dumpTo(logDir)

    summary

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "target",
      StrOption((c, s) => c.target = Some(s)),
      "set the target git version of Test262 (default: current version).",
    ),
    (
      "progress",
      BoolOption(c => c.progress = true),
      "show progress bar.",
    ),
    (
      "coverage",
      BoolOption(c => c.coverage = true),
      "measure node/branch coverage in CFG of ECMA-262.",
    ),
    (
      "timeout",
      NumOption((c, k) => c.timeLimit = Some(k)),
      "set the time limit in seconds (default: no limit).",
    ),
    (
      "log",
      BoolOption(c => c.log = true),
      "turn on logging mode.",
    ),
  )
  case class Config(
    var target: Option[String] = None,
    var coverage: Boolean = false,
    var progress: Boolean = false,
    var timeLimit: Option[Int] = None,
    var log: Boolean = false,
  )
}
