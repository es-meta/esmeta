package esmeta.analyzer

import esmeta.ESMetaTest
import esmeta.analyzer.TypeAnalyzer.Ignore

/** analyzer tests */
trait AnalyzerTest extends ESMetaTest {
  def category: String = "analyzer"
}
object AnalyzerTest {
  import ESMetaTest.*

  lazy val ignorePath = cfg.spec.manualInfo.tycheckIgnore
  lazy val ignore = ignorePath.fold(Ignore())(Ignore(_, update = true))
  lazy val analyzer: TypeAnalyzer = TypeAnalyzer(
    cfg = cfg,
    ignore = ignore,
    silent = true,
  )
}
