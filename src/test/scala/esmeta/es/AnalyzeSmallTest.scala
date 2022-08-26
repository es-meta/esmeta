package esmeta.es

import esmeta.{ESMetaTest, ES_TEST_DIR}
import esmeta.analyzer.Config
import esmeta.analyzer.domain.*
import esmeta.util.SystemUtils.*

class AnalyzeSmallTest extends ESTest {
  import ESTest.*

  Config.YET_THROW = true
  val name: String = "esAnalyzeTest"

  // registration
  def init: Unit = for (file <- walkTree(ES_TEST_DIR)) {
    val filename = file.getName
    if (jsFilter(filename))
      check(filename) { analyzeTestFile(file.toString) }
  }
  init
}
