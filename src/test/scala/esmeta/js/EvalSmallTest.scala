package esmeta.js

import esmeta.JS_TEST_DIR
import esmeta.ir.NormalInsts
import esmeta.util.SystemUtils.*

class EvalSmallTest extends JSTest {
  import JSTest.*
  val name: String = "jsEvalTest"

  // registration
  def init: Unit = for (file <- walkTree(JS_TEST_DIR)) {
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
