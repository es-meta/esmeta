package esmeta.interp

import esmeta.*
import esmeta.cfg.*
import esmeta.util.SystemUtils.*

class EvalTinyTest extends InterpTest {
  val name: String = "interpEvalTest"

  // registration
  def init: Unit = {
    for (file <- walkTree(CFG_TEST_DIR)) {
      val filename = file.getName
      if (cfgFilter(filename)) check(filename) {
        val cfgName = file.toString
        interpFile(cfgName)
      }
    }
  }
  init
}
