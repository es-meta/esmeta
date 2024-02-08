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
  lazy val defaultIgnore: Ignore = Ignore(ManualInfo.tycheckIgnore)
  lazy val defaultAnalyzer: TypeAnalyzer =
    getAnalyzer(defaultCFG, defaultIgnore)

  // helper methods
  def getIgnore(cfg: CFG): Ignore =
    cfg.spec.manualInfo.tycheckIgnore.fold(Ignore())(Ignore.apply)
  def getAnalyzer(target: String): TypeAnalyzer = getAnalyzer(getCFG(target))
  def getAnalyzer(cfg: CFG): TypeAnalyzer = getAnalyzer(cfg, getIgnore(cfg))
  def getAnalyzer(cfg: CFG, ignore: Ignore): TypeAnalyzer = TypeAnalyzer(
    cfg = cfg,
    ignore = ignore,
    silent = true,
  )
}
