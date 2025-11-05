package esmeta.test262

import esmeta.util.{ConcurrentPolicy => CP}

class EvalLargeTest extends Test262Test {
  val name: String = "test262EvalTest"

  // registration
  def init: Unit = check(name) {
    val summary = Test262.evalTest(
      concurrent = CP.Auto,
      log = true,
      verbose = true,
    )(using Test262Test.test262)
    val f = summary.failCount
    if (f > 0) fail(s"$f tests are failed.")
  }

  init
}
