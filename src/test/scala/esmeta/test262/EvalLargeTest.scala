package esmeta.test262

import esmeta.*
import esmeta.error.*
import esmeta.js.*
import esmeta.js.util.*
import esmeta.test262.NormalConfig
import esmeta.test262.util.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import java.util.concurrent.TimeoutException

class EvalLargeTest extends Test262Test {
  val name: String = "test262EvalTest"

  import Test262Test.test262.*

  // parser timeout
  val PARSE_TIMEOUT = 100 // second

  // progress bar
  lazy val progress = ProgressBar(
    msg = "Run Test262 eval test",
    iterable = config.normal,
    getName = (config, _) => config.name,
    errorHandler = (e, summary, name) =>
      e match
        case NotSupported(msg)   => summary.yets += s"$name: $msg"
        case _: TimeoutException => summary.timeouts += name
        case _: Throwable        => summary.fails += name,
  )

  // coverage
  val COVERAGE_MODE = true
  lazy val cov = Coverage(JSTest.cfg)

  // summary
  lazy val summary = progress.summary

  // registration
  def init: Unit = check(name) {
    mkdir(logDir)
    dumpFile(spec.version, s"$logDir/ecma262-version")
    dumpFile(currentVersion(BASE_DIR), s"$logDir/esmeta-version")
    summary.timeouts.setPath(s"$logDir/eval-timeout.log")
    summary.yets.setPath(s"$logDir/eval-yet.log")
    summary.fails.setPath(s"$logDir/eval-fail.log")
    summary.passes.setPath(s"$logDir/eval-pass.log")
    for (config <- progress)
      val NormalConfig(name, includes) = config
      val jsName = s"$TEST262_TEST_DIR/$name"
      if (COVERAGE_MODE) JSTest.checkExit(cov.run(jsName))
      else
        val (sourceText, ast) = loadTestFromFile(jsName) // load test
        JSTest.evalTest(sourceText, cachedAst = Some(ast)) // run interpreter
    summary.close
    val summaryStr =
      if (COVERAGE_MODE) summary.toString + LINE_SEP + cov.toString
      else summary.toString
    dumpFile(summaryStr, s"$logDir/eval-summary")
    cov.dumpTo(s"$logDir")
    if (summary.fail > 0) fail(s"${summary.fail} tests are failed.")
  }
  init
}
