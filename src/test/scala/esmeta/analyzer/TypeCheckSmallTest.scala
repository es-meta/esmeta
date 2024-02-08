package esmeta.analyzer

class TypeCheckSmallTest extends AnalyzerTest {
  import AnalyzerTest.*, analyzer.*

  val name: String = "analyzerTypeCheckTest"

  def checkAnalyzer(analyzer: TypeAnalyzer): Unit =
    analyzer.analyze
    if (analyzer.needUpdate)
      analyzer.updateIgnore
      fail(analyzer.toString)

  // registration
  def init: Unit = {
    check("es2023") { checkAnalyzer(analyzer) }
    check("default") { checkAnalyzer(defaultAnalyzer) }
  }

  init
}
