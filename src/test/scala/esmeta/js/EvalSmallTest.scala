package esmeta.js

import esmeta.JS_TEST_DIR
import esmeta.ir.NormalInsts
import esmeta.util.SystemUtils.*

class EvalSmallTest extends JSTest {
  import JSTest.*
  val name: String = "jsEvalTest"

  // TODO remove
  val IGNORE_FILES: List[String] = List(
    "await2.js",
    "class-decl1.js",
    "class-decl2.js",
    "compare1.js",
    "complement1.js",
    "delete1.js",
    "for1.js",
    "for2.js",
    "forin1.js",
    "forof1.js",
    "global-eval.js",
    "object2.js",
    "template2.js",
  )

  // registration
  def init: Unit = for (file <- walkTree(JS_TEST_DIR)) {
    val filename = file.getName
    if (jsFilter(filename) && !IGNORE_FILES.contains(filename))
      check(filename) {
        val jsName = file.toString
        val irName = js2ir(jsName)
        val insts = NormalInsts.fromFile(irName)
        checkExit(evalFile(jsName, checkAfter = insts))
      }
  }
  init
}
