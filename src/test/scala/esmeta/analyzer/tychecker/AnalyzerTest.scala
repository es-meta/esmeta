package esmeta.analyzer.tychecker

import esmeta.ESMetaTest
import esmeta.analyzer.tychecker.TyChecker
import esmeta.analyzer.tychecker.TyChecker.Ignore
import esmeta.cfg.CFG
import esmeta.util.*

/** analyzer tests */
trait AnalyzerTest extends ESMetaTest {
  def category: String = "analyzer/tychecker"
}
object AnalyzerTest {
  import ESMetaTest.*
  lazy val tychecker: TyChecker = getAnalyzer(cfg)
  inline def ignore: Ignore = ManualInfo.tycheckIgnore

  // helper methods
  def getAnalyzer(target: String): TyChecker = getAnalyzer(getCFG(target))
  def getAnalyzer(cfg: CFG): TyChecker = getAnalyzer(cfg, ignore)
  def getAnalyzer(cfg: CFG, ignore: Ignore): TyChecker = TyChecker(
    cfg = cfg,
    ignore = ignore,
    silent = true,
  )
}
