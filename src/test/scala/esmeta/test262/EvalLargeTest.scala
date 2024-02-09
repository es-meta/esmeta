package esmeta.test262

class EvalLargeTest extends Test262Test {
  val name: String = "test262EvalTest"

  // registration
  def init: Unit = check(name) {
    val summary = Test262Test.test262.evalTest(
      concurrent = true,
      verbose = true,
    )
    val f = summary.failCount
    if (f > 0) fail(s"$f tests are failed.")
  }

  init
}
