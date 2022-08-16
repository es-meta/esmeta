package esmeta.ir

import esmeta.*
import esmeta.util.SystemUtils.*

class EvalTinyTest extends IRTest {
  val name: String = "irEvalTest"

  // registration
  def init: Unit =
    for (file <- walkTree(IR_TEST_DIR)) {
      val filename = file.getName
      if (irFilter(filename)) check(filename) {
        IRTest.interpFile(file.toString)
      }
    }

  init
}
