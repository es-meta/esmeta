package esmeta.test262

class ParseLargeTest extends Test262Test {
  val name: String = "test262ParseTest"

  // registration
  def init: Unit = check(name) {
    val summary = Test262Test.test262.parseTest(
      log = true,
      useProgress = true,
      timeLimit = Some(60),
    )
    val f = summary.failCount
    if (f > 0) fail(s"$f tests are failed (See `$logDir/fail.log`).")
  }
  init
}
