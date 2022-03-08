package esmeta.test262

import esmeta.*
import esmeta.js.*
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
  lazy val progress = ProgressBar("test262 parse test", config.normal)

  // summary
  lazy val summary = progress.summary

  // registration
  def init: Unit = check(name) {
    mkdir(logDir)
    dumpFile(spec.version, s"$logDir/ecma262-version")
    dumpFile(currentVersion(BASE_DIR), s"$logDir/esmeta-version")
    summary.fails.setPath(s"$logDir/parse-fail.log")
    summary.passes.setPath(s"$logDir/parse-pass.log")
    for (config <- progress) {
      val name = config.name
      val jsName = s"$TEST262_TEST_DIR/$name"
      getError {
        timeout(JSTest.parseTest(parseFile(jsName)), PARSE_TIMEOUT)
        summary.passes += name
      }.foreach(e => {
        summary.fails += name
      })
    }
    summary.close
    dumpFile(summary, s"$logDir/parse-summary")
    if (summary.fail > 0) fail(s"${summary.fail} tests are failed.")
  }
  init
}
