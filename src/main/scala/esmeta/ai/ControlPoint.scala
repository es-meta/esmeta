package esmeta.ai

import esmeta.cfg.*

/** control points */
sealed trait ControlPoint extends AnalyzerElem {
  val view: View
  val func: Func
  def isBuiltin: Boolean = func.isBuiltin
}

/** node points */
case class NodePoint[+T <: Node](
  func: Func,
  node: T,
  view: View,
) extends ControlPoint

/** return points */
case class ReturnPoint(
  func: Func,
  view: View,
) extends ControlPoint
