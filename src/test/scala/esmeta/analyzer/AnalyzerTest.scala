package esmeta.analyzer

import esmeta.ESMetaTest
import esmeta.analyzer.TypeAnalyzer.Ignore
import esmeta.cfg.CFG
import esmeta.util.*

/** analyzer tests */
trait AnalyzerTest extends ESMetaTest {
  def category: String = "analyzer"
}
object AnalyzerTest {
  import ESMetaTest.*
  lazy val analyzer: TypeAnalyzer = getAnalyzer(cfg)
  inline def ignore: Ignore = ManualInfo.tycheckIgnore

  // helper methods
  def getAnalyzer(target: String): TypeAnalyzer = getAnalyzer(getCFG(target))
  def getAnalyzer(cfg: CFG): TypeAnalyzer = getAnalyzer(cfg, ignore)
  def getAnalyzer(cfg: CFG, ignore: Ignore): TypeAnalyzer = TypeAnalyzer(
    cfg = cfg,
    ignore = ignore,
    silent = true,
  )
}
