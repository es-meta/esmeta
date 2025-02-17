package esmeta.ty

import esmeta.cfg.*
import esmeta.ir.{Func => _, *}
import esmeta.util.*

/** type errors in specification */
sealed trait TypeError extends TyElem {
  val point: TypeErrorPoint
  inline def func: Func = point.func
}

case class ErrorDetail(
  field: String,
  actual: String,
  expected: String,
)

sealed trait DetailedTypeError extends TypeError {
  val errorDetail: List[ErrorDetail]
}

/** parameter type mismatches */
case class ParamTypeMismatch(
  point: ArgAssignPoint,
  argTy: ValueTy,
) extends TypeError

case class DetailedParamTypeMismatch(
  point: ArgAssignPoint,
  argTy: ValueTy,
  errorDetail: List[ErrorDetail],
) extends DetailedTypeError

/** return type mismatches */
case class ReturnTypeMismatch(
  point: InternalReturnPoint,
  retTy: ValueTy,
) extends TypeError

case class DetailedReturnTypeMismatch(
  point: InternalReturnPoint,
  retTy: ValueTy,
  errorDetail: List[ErrorDetail],
) extends DetailedTypeError

/** arity mismatches */
case class ArityMismatch(
  point: CallPoint,
  actual: Int,
) extends TypeError

/** invalid base in field reference errors */
case class InvalidBaseError(
  point: FieldBasePoint,
  baseTy: ValueTy,
) extends TypeError

/** operand type mismatches for unary operators */
case class UnaryOpTypeMismatch(
  point: UnaryOpPoint,
  operandTy: ValueTy,
) extends TypeError

/** operand type mismatches for binary operators */
case class BinaryOpTypeMismatch(
  point: BinaryOpPoint,
  lhsTy: ValueTy,
  rhsTy: ValueTy,
) extends TypeError

given Ordering[TypeError] = Ordering.by(e => e.point.node.id -> e.toString)

/** type error points */
sealed trait TypeErrorPoint extends TyElem {
  def func: Func
  def node: Node
  def isBuiltin: Boolean = func.isBuiltin
}

/** call points */
case class CallPoint(
  caller: Func,
  callsite: Call,
  callee: Func,
) extends TypeErrorPoint {
  inline def func = caller
  inline def node = callsite
}

/** argument assignment points */
case class ArgAssignPoint(
  callPoint: CallPoint,
  idx: Int,
) extends TypeErrorPoint {
  inline def func = callPoint.func
  inline def node = callPoint.node
  inline def param = callPoint.callee.params(idx)
}

/** internal return points */
case class InternalReturnPoint(
  func: Func,
  node: Node,
  irReturn: Return,
) extends TypeErrorPoint

/** base in field reference points */
case class FieldBasePoint(
  fieldPoint: FieldPoint,
) extends TypeErrorPoint {
  inline def func = fieldPoint.func
  inline def node = fieldPoint.node
}

/** field reference points */
case class FieldPoint(
  func: Func,
  node: Node,
  field: Field,
) extends TypeErrorPoint

/** unary operation points */
case class UnaryOpPoint(
  func: Func,
  node: Node,
  unary: EUnary,
) extends TypeErrorPoint

/** binary operation points */
case class BinaryOpPoint(
  func: Func,
  node: Node,
  binary: EBinary,
) extends TypeErrorPoint
