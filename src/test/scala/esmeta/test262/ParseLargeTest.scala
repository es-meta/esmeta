package esmeta.test262

import esmeta.*
import esmeta.es.*
import esmeta.test262.util.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*

class ParseLargeTest extends Test262Test {
  val name: String = "test262ParseTest"

  import Test262Test.test262.*

  // parser timeout
  val PARSE_TIMEOUT = 100 // second

  // progress bar
  lazy val progress = ProgressBar(
    msg = "Run Test262 parse test",
    iterable = config.normal,
    getName = (config, _) => config.name,
    errorHandler =
      (e, summary, name) => summary.fails += s"$name - ${e.getMessage}",
  )

  // summary
  lazy val summary = progress.summary

  // registration
  def init: Unit = check(name) {
    mkdir(logDir)
    dumpFile(spec.version, s"$logDir/ecma262-version")
    dumpFile(ESMeta.currentVersion, s"$logDir/esmeta-version")
    summary.fails.setPath(s"$logDir/parse-fail.log")
    summary.passes.setPath(s"$logDir/parse-pass.log")
    for (config <- progress)
      val filename = config.name
      timeout(ESTest.parseTest(ESTest.parseFile(filename)), PARSE_TIMEOUT)
    summary.close
    dumpFile(summary, s"$logDir/parse-summary")
    if (summary.fail > 0) fail(s"${summary.fail} tests are failed.")
  }
  init
}
