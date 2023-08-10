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

/** type error points */
sealed trait TypeErrorPoint[Error <: TypeError] {
  def node: Node
}

/** call points */
case class CallPoint(
  callerNp: NodePoint[Call],
  callee: Func,
) extends AnalysisPoint {
  type This = CallPoint
  inline def view = callerNp.view
  inline def func = callerNp.func
  inline def node = callerNp.node
  def withoutView: CallPoint =
    copy(callerNp = callerNp.withoutView, callee)
}

/** argument assignment points */
case class ArgAssignPoint(
  callPoint: CallPoint,
  idx: Int,
) extends AnalysisPoint
  with TypeErrorPoint[ParamTypeMismatch] {
  type This = ArgAssignPoint
  inline def view = callPoint.view
  inline def func = callPoint.func
  inline def node = callPoint.node
  inline def param = callPoint.callee.params(idx)
  def withoutView: ArgAssignPoint = copy(callPoint = callPoint.withoutView)
}

/** internal return points */
case class InternalReturnPoint(
  returnNp: NodePoint[Node],
  irReturn: Return,
) extends AnalysisPoint
  with TypeErrorPoint[ReturnTypeMismatch] {
  type This = InternalReturnPoint
  inline def view = returnNp.view
  inline def func = returnNp.func
  inline def node = returnNp.node
  def withoutView: InternalReturnPoint = copy(returnNp = returnNp.withoutView)
}

/** return-if-abrupt points */
case class ReturnIfAbruptPoint(
  np: NodePoint[Node],
  riaExpr: EReturnIfAbrupt,
) extends AnalysisPoint
  with TypeErrorPoint[UncheckedAbruptComp] {
  type This = ReturnIfAbruptPoint
  inline def view = np.view
  inline def func = np.func
  inline def node = np.node
  def withoutView: ReturnIfAbruptPoint = copy(np = np.withoutView)
}

/** base in property reference points */
case class PropBasePoint(
  propPoint: PropPoint,
) extends AnalysisPoint
  with TypeErrorPoint[InvalidPropBase] {
  type This = PropBasePoint
  inline def view = propPoint.view
  inline def func = propPoint.func
  inline def node = propPoint.node
  def withoutView: PropBasePoint = copy(propPoint = propPoint.withoutView)
}

/** property reference points */
case class PropPoint(
  np: NodePoint[Node],
  prop: Prop,
) extends AnalysisPoint {
  type This = PropPoint
  inline def view = np.view
  inline def func = np.func
  inline def node = np.node
  def withoutView: PropPoint = copy(np = np.withoutView)
}

/** unary operation points */
case class UnaryOpPoint(
  np: NodePoint[Node],
  unary: EUnary,
) extends AnalysisPoint
  with TypeErrorPoint[UnaryOpTypeMismatch] {
  type This = UnaryOpPoint
  inline def view = np.view
  inline def func = np.func
  inline def node = np.node
  def withoutView: UnaryOpPoint = copy(np = np.withoutView)
}

/** binary operation points */
case class BinaryOpPoint(
  np: NodePoint[Node],
  binary: EBinary,
) extends AnalysisPoint
  with TypeErrorPoint[BinaryOpTypeMismatch] {
  type This = BinaryOpPoint
  inline def view = np.view
  inline def func = np.func
  inline def node = np.node
  def withoutView: BinaryOpPoint = copy(np = np.withoutView)
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
