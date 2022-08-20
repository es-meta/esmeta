package esmeta.interpreter

import esmeta.*
import esmeta.util.SystemUtils.*

/** eval test */
class EvalTinyTest extends InterpreterTest {
  val name: String = "interpreterEvalTest"

  // registration
  def init: Unit =
    for (file <- walkTree(IR_TEST_DIR)) {
      val filename = file.getName
      if (irFilter(filename)) check(filename) {
        InterpreterTest.interpFile(file.toString)
      }
    }

  init
}
