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

  /** return-if-abrupt points */
  case class ReturnIfAbruptPoint(
    np: NodePoint[Node],
    riaExpr: EReturnIfAbrupt,
  ) extends TypeErrorPoint {
    inline def view = np.view
    inline def func = np.func
    inline def node = np.node
  }

  /** base in property reference points */
  case class PropBasePoint(
    propPoint: PropPoint,
  ) extends TypeErrorPoint {
    inline def view = propPoint.view
    inline def func = propPoint.func
    inline def node = propPoint.node
  }

  /** property reference points */
  case class PropPoint(
    np: NodePoint[Node],
    prop: Prop,
  ) extends TypeErrorPoint {
    inline def view = np.view
    inline def func = np.func
    inline def node = np.node
  }

  /** unary operation points */
  case class UnaryOpPoint(
    np: NodePoint[Node],
    unary: EUnary,
  ) extends TypeErrorPoint {
    inline def view = np.view
    inline def func = np.func
    inline def node = np.node
  }

  /** binary operation points */
  case class BinaryOpPoint(
    np: NodePoint[Node],
    binary: EBinary,
  ) extends TypeErrorPoint {
    inline def view = np.view
    inline def func = np.func
    inline def node = np.node
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
