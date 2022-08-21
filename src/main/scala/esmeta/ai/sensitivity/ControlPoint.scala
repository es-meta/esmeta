package esmeta.ai.sensitivity

import esmeta.cfg.*

/** control points */
sealed trait ControlPoint {
  val view: View
  val func: Func
  // def isBuiltin: Boolean = func.isBuiltin // XXX required?
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
