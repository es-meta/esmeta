package esmeta.analyzer

import esmeta.cfg.*
import esmeta.ir.{Func => _, *}
import esmeta.ty.*
import esmeta.util.*

trait TypeErrorDecl { self: Analyzer =>

  /** type errors in specification */
  sealed trait TypeError extends AnalyzerElem {
    val point: TypeErrorPoint
    inline def func: Func = point.func
  }

  /** parameter type mismatches */
  case class ParamTypeMismatch(
    point: ArgAssignPoint,
    argTy: ValueTy,
  ) extends TypeError

  /** return type mismatches */
  case class ReturnTypeMismatch(
    point: InternalReturnPoint,
    retTy: ValueTy,
  ) extends TypeError

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
  sealed trait TypeErrorPoint extends AnalyzerElem {
    def view: View
    def func: Func
    def node: Node
    def noView: TypeErrorPoint
    def isBuiltin: Boolean = func.isBuiltin
  }

  /** call points */
  case class CallPoint(
    callerNp: NodePoint[Call],
    callee: Func,
  ) extends TypeErrorPoint {
    inline def view = callerNp.view
    inline def func = callerNp.func
    inline def node = callerNp.node
    inline def noView: CallPoint = copy(callerNp = callerNp.noView)
  }

  /** argument assignment points */
  case class ArgAssignPoint(
    callPoint: CallPoint,
    idx: Int,
  ) extends TypeErrorPoint {
    inline def view = callPoint.view
    inline def func = callPoint.func
    inline def node = callPoint.node
    inline def param = callPoint.callee.params(idx)
    inline def noView: ArgAssignPoint = copy(callPoint = callPoint.noView)
  }

  /** internal return points */
  case class InternalReturnPoint(
    returnNp: NodePoint[Node],
    irReturn: Return,
  ) extends TypeErrorPoint {
    inline def view = returnNp.view
    inline def func = returnNp.func
    inline def node = returnNp.node
    inline def noView: InternalReturnPoint = copy(returnNp = returnNp.noView)
  }

  /** base in field reference points */
  case class FieldBasePoint(
    fieldPoint: FieldPoint,
  ) extends TypeErrorPoint {
    inline def view = fieldPoint.view
    inline def func = fieldPoint.func
    inline def node = fieldPoint.node
    inline def noView: FieldBasePoint = copy(fieldPoint = fieldPoint.noView)
  }

  /** field reference points */
  case class FieldPoint(
    nodePoint: NodePoint[Node],
    field: Field,
  ) extends TypeErrorPoint {
    inline def view = nodePoint.view
    inline def func = nodePoint.func
    inline def node = nodePoint.node
    inline def noView: FieldPoint = copy(nodePoint = nodePoint.noView)
  }

  /** unary operation points */
  case class UnaryOpPoint(
    nodePoint: NodePoint[Node],
    unary: EUnary,
  ) extends TypeErrorPoint {
    inline def view = nodePoint.view
    inline def func = nodePoint.func
    inline def node = nodePoint.node
    inline def noView: UnaryOpPoint = copy(nodePoint = nodePoint.noView)
  }

  /** binary operation points */
  case class BinaryOpPoint(
    nodePoint: NodePoint[Node],
    binary: EBinary,
  ) extends TypeErrorPoint {
    inline def view = nodePoint.view
    inline def func = nodePoint.func
    inline def node = nodePoint.node
    inline def noView: BinaryOpPoint = copy(nodePoint = nodePoint.noView)
  }
}
