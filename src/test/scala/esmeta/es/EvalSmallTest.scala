package esmeta.es

import esmeta.ES_TEST_DIR
import esmeta.ir.NormalInsts
import esmeta.util.SystemUtils.*

/** ECMAScript eval test */
class EvalSmallTest extends ESTest {
  import ESTest.*
  val name: String = "esEvalTest"

  // registration
  def init: Unit = for (file <- walkTree(ES_TEST_DIR)) {
    val filename = file.getName
    if (jsFilter(filename))
      check(filename) {
        val jsName = file.toString
        val irName = js2ir(jsName)
        val insts = NormalInsts.fromFile(irName)
        checkExit(evalFile(jsName, checkAfter = insts))
      }
  }
  init
}
