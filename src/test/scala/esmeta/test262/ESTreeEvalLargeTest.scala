package esmeta.test262

import esmeta.util.{ConcurrentPolicy => CP}

/** runs Test262 with the programs parsed through ESTree
  *
  * The AST equivalence of [[ESTreeLargeTest]] covers only the parsing itself;
  * this test also evaluates the tests, so that everything the specification
  * does with an AST afterwards -- reading source text back, re-parsing covered
  * productions, resolving early errors -- runs on trees the ESTree parser
  * built.
  */
class ESTreeEvalLargeTest extends Test262Test {
  val name: String = "test262ESTreeEvalTest"

  // registration
  def init: Unit = check(name) {
    val summary = Test262.evalTest(
      concurrent = CP.Auto,
      log = true,
      verbose = true,
    )(using ESTreeEvalLargeTest.test262)
    val f = summary.failCount
    if (f > 0) fail(s"$f tests are failed.")
  }
  init
}
object ESTreeEvalLargeTest {
  lazy val test262 =
    Test262(Test262.currentVersion, esmeta.ESMetaTest.cfg, useESTree = true)
}
