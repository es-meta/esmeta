package esmeta.test262

import esmeta.*
import esmeta.es.*
import esmeta.test262.util.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*

class ParseLargeTest extends Test262Test {
  val name: String = "test262ParseTest"

  val test262 = Test262Test.test262

  // time limit
  private val timeLimit = Some(100) // seconds

  // get all applicable tests with progress bar
  lazy val tests = ProgressBar(
    msg = "Run Test262 parse test",
    iterable = test262.config.normal,
    getName = (config, _) => config.name,
    errorHandler =
      (e, summary, name) => summary.fails += s"$name - ${e.getMessage}",
  )

  // test progress summary
  lazy val summary = tests.summary

  // registration
  def init: Unit = check(name) {
    mkdir(logDir)
    dumpFile(spec.versionString, s"$logDir/ecma262-version")
    dumpFile(ESMeta.currentVersion, s"$logDir/esmeta-version")
    summary.timeouts.setPath(s"$logDir/timeout.log")
    summary.yets.setPath(s"$logDir/yet.log")
    summary.fails.setPath(s"$logDir/fail.log")
    summary.passes.setPath(s"$logDir/pass.log")
    for (test <- tests)
      val NormalConfig(filename, _) = test
      timeout(ESTest.parseTest(ESTest.parseFile(filename)), timeLimit)
    summary.close
    dumpFile(summary, s"$logDir/summary")
    val f = summary.fail
    if (f > 0) fail(s"$f tests are failed (See `$logDir/fail.log`).")
  }
  init
}
