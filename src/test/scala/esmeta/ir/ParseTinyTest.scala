package esmeta.ir

import esmeta.IR_TEST_DIR
import esmeta.ir._
import esmeta.util.FileUtils._

class ParseTinyTest extends IRTest {
  val name: String = "irParseTest"

  // registration
  def init: Unit = {}

  // TODO for (file <- walkTree(IR_TEST_DIR)) {
  //   val filename = file.getName
  //   if (irFilter(filename)) check(filename, {
  //     val name = file.toString
  //     irParseTestFile(name)
  //   })
  // }
  init
}
