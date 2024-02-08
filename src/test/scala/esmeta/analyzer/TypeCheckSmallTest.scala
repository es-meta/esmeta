package esmeta.analyzer

class TypeCheckSmallTest extends AnalyzerTest {
  import AnalyzerTest.*, analyzer.*

  val name: String = "analyzerTypeCheckTest"

  // registration
  def init: Unit = {
    check("recent") {
      assert(analyzer.detected.isEmpty)
      assert(analyzer.unusedSet.isEmpty)
      if (analyzer.needUpdate) analyzer.updateIgnore
    }
  }

  init
}
