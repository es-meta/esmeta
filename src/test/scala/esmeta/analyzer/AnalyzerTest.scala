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
  lazy val ignore = ignorePath.fold(Ignore())(Ignore.apply)
  lazy val analyzer: TypeAnalyzer = TypeAnalyzer(
    cfg = cfg,
    ignore = ignore,
    silent = true,
  )
}
