package esmeta.analyzer

import esmeta.analyzer.domain.Type
import esmeta.cfg.*

/** view abstraction for analysis sensitivities */
case class View(
  calls: List[Call] = Nil,
  loops: List[LoopCtxt] = Nil,
  intraLoopDepth: Int = 0,
  tys: List[Type] = Nil,
) extends AnalyzerElem

/** loop context */
case class LoopCtxt(loop: Branch, depth: Int)
