package esmeta.analyzer

import esmeta.cfg.*

/** control points */
sealed trait ControlPoint extends AnalyzerElem {
  val view: View
  def func: Func
}
case class NodePoint[+T <: Node](node: T, view: View) extends ControlPoint {
  def func: Func = ??? // TODO
}
case class ReturnPoint(func: Func, view: View) extends ControlPoint
