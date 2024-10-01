package esmeta.analyzer

import esmeta.cfg.*
import esmeta.ir.{Func => _, *}

trait ControlPointDecl { self: Analyzer =>

  /** control points */
  sealed trait ControlPoint extends AnalyzerElem {
    def view: View
    def func: Func
    def isBuiltin: Boolean = func.isBuiltin
    def toReturnPoint: ReturnPoint = this match
      case np: NodePoint[Node] => ReturnPoint(np.func, np.view)
      case rp: ReturnPoint     => rp
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

  given Ordering[ControlPoint] = Ordering.by(_ match
    case NodePoint(f, n, _) => (f.id, n.id)
    case ReturnPoint(f, _)  => (f.id, Int.MaxValue),
  )
}
