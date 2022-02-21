package esmeta.interp

import esmeta.*
import esmeta.cfg.*
import esmeta.util.SystemUtils.*

class EvalTinyTest extends InterpTest {
  val name: String = "interpEvalTest"

  // registration
  def init: Unit = {
    // check interpretation tests for IR files
    for (file <- walkTree(IR_TEST_DIR)) {
      val filename = file.getName
      if (irFilter(filename)) check(filename) {
        InterpTest.interpFile(file.toString)
      }
    }
  }
  init
}
