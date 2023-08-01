package esmeta.analyzer

import esmeta.ir.{Func => _, *}
import esmeta.cfg.*
import esmeta.analyzer.domain.AllocSite

/** analysis points */
sealed trait AnalysisPoint extends AnalyzerElem {
  def view: View
  def func: Func
  def isBuiltin: Boolean = func.isBuiltin
  def toReturnPoint: ReturnPoint = ReturnPoint(func, view)
  def withoutView: AnalysisPoint
}

/** call points */
case class CallPoint[+T <: Node](
  callerNp: NodePoint[Call],
  calleeNp: NodePoint[T],
) extends AnalysisPoint {
  inline def view = calleeNp.view
  inline def func = calleeNp.func
  def withoutView: CallPoint[T] =
    copy(calleeNp = calleeNp.withoutView, callerNp = callerNp.withoutView)
}

/** argument assignment points */
case class ArgAssignPoint[+T <: Node](
  cp: CallPoint[T],
  idx: Int,
) extends AnalysisPoint {
  inline def view = cp.view
  inline def func = cp.func
  inline def param = cp.calleeNp.func.params(idx)
  def withoutView: ArgAssignPoint[T] = copy(cp = cp.withoutView)
}

/** internal return points */
case class InternalReturnPoint(
  irReturn: Return,
  calleeRp: ReturnPoint,
) extends AnalysisPoint {
  inline def view = calleeRp.view
  inline def func = calleeRp.func
  def withoutView: InternalReturnPoint =
    copy(calleeRp = calleeRp.withoutView)
}

/** return-if-abrupt points */
case class ReturnIfAbruptPoint(
  cp: ControlPoint,
  riaExpr: EReturnIfAbrupt,
) extends AnalysisPoint {
  inline def view = cp.view
  inline def func = cp.func
  def withoutView: ReturnIfAbruptPoint = copy(cp = cp.withoutView)
}

case class MapAllocPoint(cp: ControlPoint, emap: EMap) extends AnalysisPoint {
  inline def view = cp.view
  inline def func = cp.func
  def withoutView: MapAllocPoint = copy(cp = cp.withoutView)
}

/** property lookup points */
case class PropertyLookupPoint(
  cp: ControlPoint,
  ref: Option[Ref],
) extends AnalysisPoint {
  inline def view = cp.view
  inline def func = cp.func
  def withoutView: PropertyLookupPoint = copy(cp = cp.withoutView)
}

/** property assign points */
case class PropertyAssignPoint(
  cp: ControlPoint,
  ref: Option[Ref],
) extends AnalysisPoint {
  inline def view = cp.view
  inline def func = cp.func
  def withoutView: PropertyAssignPoint = copy(cp = cp.withoutView)
}

/** detailed lookup kinds */
enum LookupKind:
  case Ast, Str, Name, Comp, Record, List, Symbol, SubMap

/** control points */
sealed trait ControlPoint extends AnalysisPoint {
  def withoutView: ControlPoint
}

/** node points */
case class NodePoint[+T <: Node](
  func: Func,
  node: T,
  view: View,
) extends ControlPoint {
  def withoutView: NodePoint[T] = copy(view = View())
}

/** return points */
case class ReturnPoint(
  func: Func,
  view: View,
) extends ControlPoint {
  def withoutView: ReturnPoint = copy(view = View())
}
