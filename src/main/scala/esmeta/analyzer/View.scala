package esmeta.analyzer

import esmeta.cfg.*
import esmeta.ir.Type

/** view abstraction for analysis sensitivities */
case class View(
  calls: List[Call] = Nil,
  loops: List[LoopCtxt] = Nil,
  intraLoopDepth: Int = 0,
) extends AnalyzerElem

/** loop context */
case class LoopCtxt(loop: Branch, depth: Int)
