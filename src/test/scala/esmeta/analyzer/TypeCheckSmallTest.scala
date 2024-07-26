package esmeta.analyzer

class TypeCheckSmallTest extends AnalyzerTest {
  import AnalyzerTest.*, analyzer.*

  val name: String = "analyzerTypeCheckTest"

  // registration
  def init: Unit =
    // TODO revert
    // check("typeCheck") {
    //   analyzer.analyze
    //   if (analyzer.needUpdate)
    //     analyzer.updateIgnore
    //     println(s"type checker failed to analyze ${analyzer.cfg.spec.version}")
    //     fail(analyzer.toString)
    // }
    ()

  init
}
