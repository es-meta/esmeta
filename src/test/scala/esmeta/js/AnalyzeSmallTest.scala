package esmeta.js

import esmeta.JS_TEST_DIR
import esmeta.util.SystemUtils.*

class AnalyzeSmallTest extends JSTest {
  import JSTest.*
  val name: String = "jsAnalyzeTest"

  // registration
  def init: Unit = for (file <- walkTree(JS_TEST_DIR)) {
    val filename = file.getName
    if (jsFilter(filename))
      check(filename) { analyzeTestFile(file.toString, 1) }
  }
  init
}
