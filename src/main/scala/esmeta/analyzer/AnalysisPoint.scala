package esmeta.analyzer

import esmeta.cfg.*
import esmeta.error.InvalidAnalysisPointMerge
import esmeta.ir.{Func => _, *}

/** analysis points */
sealed trait AnalysisPoint extends AnalyzerElem {
  type This <: AnalysisPoint
  def view: View
  def func: Func
  def isBuiltin: Boolean = func.isBuiltin
  def toReturnPoint: ReturnPoint = ReturnPoint(func, view)
  def withoutView: This
  def +(that: This): This =
    if (this == that) that
    else if (this.withoutView == that.withoutView) this.withoutView
    else throw InvalidAnalysisPointMerge(this, that)
}

/** call points */
case class CallPoint(
  callerNp: NodePoint[Call],
  callee: Func,
) extends AnalysisPoint {
  type This = CallPoint
  inline def view = callerNp.view
  inline def func = callerNp.func
  def withoutView: CallPoint =
    copy(callerNp = callerNp.withoutView, callee)
}

/** argument assignment points */
case class ArgAssignPoint(
  callPoint: CallPoint,
  idx: Int,
) extends AnalysisPoint {
  type This = ArgAssignPoint
  inline def view = callPoint.view
  inline def func = callPoint.func
  inline def param = callPoint.callee.params(idx)
  def withoutView: ArgAssignPoint = copy(callPoint = callPoint.withoutView)
}

/** internal return points */
case class InternalReturnPoint(
  calleeRp: ReturnPoint,
  irReturn: Return,
) extends AnalysisPoint {
  type This = InternalReturnPoint
  inline def view = calleeRp.view
  inline def func = calleeRp.func
  def withoutView: InternalReturnPoint = copy(calleeRp = calleeRp.withoutView)
}

/** return-if-abrupt points */
case class ReturnIfAbruptPoint(
  cp: ControlPoint,
  riaExpr: EReturnIfAbrupt,
) extends AnalysisPoint {
  type This = ReturnIfAbruptPoint
  inline def view = cp.view
  inline def func = cp.func
  def withoutView: ReturnIfAbruptPoint = copy(cp = cp.withoutView)
}

/** base in property reference points */
case class PropBasePoint(
  propPoint: PropPoint,
) extends AnalysisPoint {
  type This = PropBasePoint
  inline def view = propPoint.view
  inline def func = propPoint.func
  def withoutView: PropBasePoint = copy(propPoint = propPoint.withoutView)
}

/** property reference points */
case class PropPoint(
  cp: ControlPoint,
  prop: Prop,
) extends AnalysisPoint {
  type This = PropPoint
  inline def view = cp.view
  inline def func = cp.func
  def withoutView: PropPoint = copy(cp = cp.withoutView)
}

/** unary operation points */
case class UnaryOpPoint(
  cp: ControlPoint,
  unary: EUnary,
) extends AnalysisPoint {
  type This = UnaryOpPoint
  inline def view = cp.view
  inline def func = cp.func
  def withoutView: UnaryOpPoint = copy(cp = cp.withoutView)
}

/** binary operation points */
case class BinaryOpPoint(
  cp: ControlPoint,
  binary: EBinary,
) extends AnalysisPoint {
  type This = BinaryOpPoint
  inline def view = cp.view
  inline def func = cp.func
  def withoutView: BinaryOpPoint = copy(cp = cp.withoutView)
}

/** control points */
sealed trait ControlPoint extends AnalysisPoint { type This <: ControlPoint }

/** node points */
case class NodePoint[+T <: Node](
  func: Func,
  node: T,
  view: View,
) extends ControlPoint {
  type This = NodePoint[Node]
  def withoutView: NodePoint[T] = copy(view = View())
}

/** return points */
case class ReturnPoint(
  func: Func,
  view: View,
) extends ControlPoint {
  type This = ReturnPoint
  def withoutView: ReturnPoint = copy(view = View())
}
