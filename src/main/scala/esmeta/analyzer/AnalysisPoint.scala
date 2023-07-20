package esmeta.analyzer

import esmeta.ir.{Func => _, *}
import esmeta.cfg.*

/** analysis points */
sealed trait AnalysisPoint extends AnalyzerElem {
  def view: View
  def func: Func
  def isBuiltin: Boolean = func.isBuiltin
  def toReturnPoint: ReturnPoint = ReturnPoint(func, view)
}

/** call points */
case class CallPoint[+T <: Node](
  callerNp: NodePoint[Call],
  calleeNp: NodePoint[T],
) extends AnalysisPoint {
  inline def view = calleeNp.view
  inline def func = calleeNp.func
}

/** argument assignment points */
case class ArgAssignPoint[+T <: Node](
  cp: CallPoint[T],
  idx: Int,
) extends AnalysisPoint {
  inline def view = cp.view
  inline def func = cp.func
  inline def param = cp.calleeNp.func.params(idx)
}

/** internal return points */
case class InternalReturnPoint(
  irReturn: Return,
  calleeRp: ReturnPoint,
) extends AnalysisPoint {
  inline def view = calleeRp.view
  inline def func = calleeRp.func
}

case class ReturnIfAbruptPoint(
  cp: ControlPoint,
  riaExpr: EReturnIfAbrupt,
) extends AnalysisPoint {
  inline def view = cp.view
  inline def func = cp.func
}

/** control points */
sealed trait ControlPoint extends AnalysisPoint

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
