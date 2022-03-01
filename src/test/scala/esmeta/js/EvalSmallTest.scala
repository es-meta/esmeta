package esmeta.js

import esmeta.JS_TEST_DIR
import esmeta.ir.NormalInsts
import esmeta.util.SystemUtils.*

class EvalSmallTest extends JSTest {
  import JSTest.*
  val name: String = "jsEvalTest"

  // TODO remove
  val IGNORE_FILES: List[String] = List(
    // "abstract-equality2.js",
    // "abstract-equality3.js",
    // "abstract-equality4.js",
    // "abstract-equality5.js",
    // "addition1.js",
    // "addition2.js",
    // "addition3.js",
    // "addition4.js",
    // "addition5.js",
    // "addition6.js",
    // "arguments1.js",
    // "arguments2.js",
    // "array1.js",
    // "array2.js",
    // "array3.js",
    // "array4.js",
    "array5.js",
    "array6.js",
    // "assign-object.js",
    // "assign-object2.js",
    "assign-object3.js",
    "assign-object4.js",
    "assign-object5.js",
    "assign-object6.js",
    "assignment1.js",
    "async-generator1.js",
    "async1.js",
    "await1.js",
    "await2.js",
    "bind1.js",
    "block1.js",
    // "boolean1.js",
    // "boolean2.js",
    // "boolean3.js",
    "boolean4.js",
    "class-decl1.js",
    "class-decl2.js",
    "compare1.js",
    "complement1.js",
    "constructor1.js",
    "delete1.js",
    // "empty-statement.js",
    // "equality1.js",
    // "expression-statement1.js",
    "for1.js",
    "for2.js",
    "forin1.js",
    "forof1.js",
    // "function-application1.js",
    // "function-application2.js",
    // "function-application3.js",
    // "function-application4.js",
    "function-application5.js",
    // "function-declare1.js",
    "function1.js",
    "function2.js",
    "function3.js",
    "generator1.js",
    "generator2.js",
    "global-eval.js",
    // "global-object.js",
    // "global-object2.js",
    // "global.js",
    "harness.js",
    // "if-statement1.js",
    // "increment1.js",
    // "increment2.js",
    // "instanceof1.js",
    "instanceof2.js",
    // "multiple-statements.js",
    // "number1.js",
    "number2.js",
    "number3.js",
    // "number4.js",
    "object1.js",
    "object2.js",
    "promise1.js",
    // "read-property1.js",
    "semicolon-insertion1.js",
    // "semicolon-insertion2.js",
    // "semicolon-insertion3.js",
    "set1.js",
    "shift1.js",
    // "string1.js",
    "string2.js",
    // "string3.js",
    // "switch1.js",
    // "symbol1.js",
    // "symbol2.js",
    // "symbol3.js",
    "template1.js",
    "template2.js",
    // "try1.js",
    // "try2.js",
    // "typeof1.js",
    // "variable-declare1.js",
    // "variable-declare2.js",
    // "variable-declare3.js",
    // "variable-declare4.js",
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
