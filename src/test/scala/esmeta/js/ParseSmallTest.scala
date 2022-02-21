package esmeta.js

import esmeta.JS_TEST_DIR
import esmeta.util.SystemUtils.*

class ParseSmallTest extends JSTest {
  val name: String = "jsParseTest"

  // registration
  def init: Unit = {
    // check parser and stringifier from files
    for (file <- walkTree(JS_TEST_DIR)) {
      val filename = file.getName
      if (jsFilter(filename)) check(filename) {
        val name = file.toString
        JSTest.parseFileTest(name)
      }
    }
  }

  init
}
