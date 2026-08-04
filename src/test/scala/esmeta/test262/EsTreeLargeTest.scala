package esmeta.test262

import esmeta.util.{ConcurrentPolicy => CP}

/** compares the ESTree parser with the grammar of ECMA-262 on Test262 */
class EsTreeLargeTest extends Test262Test {
  val name: String = "test262EsTreeTest"

  // registration
  def init: Unit = check(name) {
    val summary = Test262.estreeTest(
      concurrent = CP.Auto,
      log = true,
      verbose = true,
    )(using Test262Test.test262)
    val f = summary.failCount
    if (f > 0) fail(s"$f tests are failed.")
  }
  init
}
