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
  lazy val progress = ProgressBar("test262 eval test", config.normal)
  // lazy val progress = ProgressBar("test262 eval test", manualConfig.normal)

  // coverage
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
    for (config <- progress) {
      val NormalConfig(name, includes) = config
      val jsName = s"$TEST262_TEST_DIR/$name"
      getError {
        // run interp and measure coverage
        val st = cov.run(jsName, fromTest262 = true)
        // check exit
        JSTest.checkExit(st)
        summary.passes += name
      }.foreach {
        case NotSupported(msg)   => summary.yets += s"$name: $msg"
        case _: TimeoutException => summary.timeouts += name
        case _: Throwable        => summary.fails += name
      }
    }
    summary.close
    dumpFile(summary, s"$logDir/eval-summary")
    cov.dumpTo(s"$logDir/coverage")
    if (summary.fail > 0) fail(s"${summary.fail} tests are failed.")
  }
  init
}
