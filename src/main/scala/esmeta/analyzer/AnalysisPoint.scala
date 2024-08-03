package esmeta.analyzer

import esmeta.cfg.*
import esmeta.ir.{Func => _, *}

trait AnalysisPointDecl { self: Analyzer =>

  /** analysis points */
  sealed trait AnalysisPoint extends AnalyzerElem {
    def view: View
    def func: Func
    def isBuiltin: Boolean = func.isBuiltin
  }

  /** type error points */
  sealed trait TypeErrorPoint extends AnalysisPoint {
    def node: Node
  }

  /** call points */
  case class CallPoint(
    callerNp: NodePoint[Call],
    callee: Func,
  ) extends TypeErrorPoint {
    inline def view = callerNp.view
    inline def func = callerNp.func
    inline def node = callerNp.node
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
  }

  /** internal return points */
  case class InternalReturnPoint(
    returnNp: NodePoint[Node],
    irReturn: Return,
  ) extends TypeErrorPoint {
    inline def view = returnNp.view
    inline def func = returnNp.func
    inline def node = returnNp.node
  }

  /** base in field reference points */
  case class FieldBasePoint(
    fieldPoint: FieldPoint,
  ) extends TypeErrorPoint {
    inline def view = fieldPoint.view
    inline def func = fieldPoint.func
    inline def node = fieldPoint.node
  }

  /** field reference points */
  case class FieldPoint(
    nodePoint: NodePoint[Node],
    field: Field,
  ) extends TypeErrorPoint {
    inline def view = nodePoint.view
    inline def func = nodePoint.func
    inline def node = nodePoint.node
  }

  /** unary operation points */
  case class UnaryOpPoint(
    nodePoint: NodePoint[Node],
    unary: EUnary,
  ) extends TypeErrorPoint {
    inline def view = nodePoint.view
    inline def func = nodePoint.func
    inline def node = nodePoint.node
  }

  /** binary operation points */
  case class BinaryOpPoint(
    nodePoint: NodePoint[Node],
    binary: EBinary,
  ) extends TypeErrorPoint {
    inline def view = nodePoint.view
    inline def func = nodePoint.func
    inline def node = nodePoint.node
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
}
