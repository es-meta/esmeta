package esmeta.es

import esmeta.ES_TEST_DIR
import esmeta.util.SystemUtils.*

class ParseSmallTest extends ESTest {
  val name: String = "esParseTest"

  // registration
  def init: Unit = {
    // check parser and stringifier from files
    for (file <- walkTree(ES_TEST_DIR)) {
      val filename = file.getName
      if (jsFilter(filename)) check(filename) {
        val name = file.toString
        ESTest.parseFileTest(name)
      }
    }
  }

  init
}
