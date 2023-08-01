package esmeta.analyzer

import esmeta.cfg.*
import esmeta.ty.*

/** view abstraction for analysis sensitivities */
case class View(
  calls: List[Call] = Nil,
  loops: List[LoopCtxt] = Nil,
  intraLoopDepth: Int = 0,
  tys: List[ValueTy] = Nil,
) extends AnalyzerElem {

  /** empty check */
  def isEmpty: Boolean = this == View()
}

/** loop context */
case class LoopCtxt(loop: Branch, depth: Int)
