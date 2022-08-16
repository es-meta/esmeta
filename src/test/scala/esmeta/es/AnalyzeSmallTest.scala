package esmeta.es

import esmeta.ES_TEST_DIR
import esmeta.util.SystemUtils.*

class AnalyzeSmallTest extends ESTest {
  import ESTest.*
  val name: String = "esAnalyzeTest"

  // registration
  def init: Unit = for (file <- walkTree(ES_TEST_DIR)) {
    val filename = file.getName
    if (jsFilter(filename))
      check(filename) { analyzeTestFile(file.toString) }
  }
  init
}
