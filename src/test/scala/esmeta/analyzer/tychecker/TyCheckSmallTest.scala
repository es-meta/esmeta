package esmeta.analyzer.tychecker

class TyCheckSmallTest extends AnalyzerTest {
  import AnalyzerTest.*

  val name: String = "analyzerTyCheckTest"

  // registration
  def init: Unit = check("typeCheck") {
    tychecker.analyze
    if (tychecker.needUpdate)
      tychecker.updateIgnore
      println(s"type checker failed to analyze ${tychecker.cfg.spec.version}")
      fail(tychecker.toString)
  }

  init
}
