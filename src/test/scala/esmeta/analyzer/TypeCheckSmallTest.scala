package esmeta.analyzer

class TypeCheckSmallTest extends AnalyzerTest {
  import AnalyzerTest.*, analyzer.*

  val name: String = "analyzerTypeCheckTest"

  // registration
  def init: Unit = {
    check("es2023") {
      if (analyzer.needUpdate)
        analyzer.updateIgnore
        assert(analyzer.detected.isEmpty)
        assert(analyzer.unusedSet.isEmpty)
    }
  }

  init
}
