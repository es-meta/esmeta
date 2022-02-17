package esmeta.interp

import esmeta.*
import esmeta.cfg.*
import esmeta.util.SystemUtils.*

class EvalTinyTest extends InterpTest {
  val name: String = "interpEvalTest"

  // registration
  def init: Unit = {
    for (file <- walkTree(IR_TEST_DIR)) {
      val filename = file.getName
      if (irFilter(filename)) check(filename) {
        interpFile(file.toString)
      }
    }
  }
  init
}
