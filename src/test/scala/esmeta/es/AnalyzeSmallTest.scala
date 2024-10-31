package esmeta.es

import esmeta.{ESMetaTest, ES_TEST_DIR}
import esmeta.analyzer.*
import esmeta.util.SystemUtils.*

class AnalyzeSmallTest extends ESTest {
  import ESTest.*

  val name: String = "esAnalyzeTest"

  // registration
  def init: Unit =
    // TODO revert
    // for (file <- walkTree(ES_TEST_DIR)) {
    //   val filename = file.getName
    //   if (jsFilter(filename))
    //     check(filename) { analyzeTestFile(file.toString) }
    // }
    // init
    ()
}
