package esmeta.analyzer

import esmeta.ESMetaTest

/** analyzer tests */
trait AnalyzerTest extends ESMetaTest {
  def category: String = "analyzer"
}
object AnalyzerTest {
  import ESMetaTest.*

  lazy val analyzer: Analyzer = TypeAnalyzer(cfg)
}
