package esmeta.analyzer.propflow

import esmeta.cfg.{util => _, *}
import esmeta.ir.{Func => _, util => _, *}
import esmeta.state.*
import esmeta.ty.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import scala.annotation.tailrec
import esmeta.es.builtin.JOB_QUEUE

trait AbsTransferDecl { analyzer: PropFlowAnalyzer =>

  /** abstract transfer function */
  class AbsTransfer extends AbsTransferLike {

    /** loading monads */
    import monad.*

    /** loading predefined abstract values */
    import AbsValue.*

    // =========================================================================
    // Implementation for General AbsTransfer
    // =========================================================================
    /** transfer function for node points */
    def apply(np: NodePoint[_]): Unit =
      val st = getResult(np)
      given NodePoint[_] = np
      given AbsState = st
      val NodePoint(func, node, _) = np
      node match
        case Block(_, insts, next) =>
          val newSt = insts.foldLeft(st) { (nst, inst) => transfer(inst)(nst) }
          next.foreach(to => analyzer += getNextNp(np, to) -> newSt)
        case call: Call =>
          call.next.foreach(to => analyzer += getNextNp(np, to) -> st)
        case br @ Branch(_, kind, c, _, thenNode, elseNode) =>
          thenNode.map(analyzer += getNextNp(np, _) -> st)
          elseNode.map(analyzer += getNextNp(np, _) -> st)

    /** transfer function for return points */
    def apply(rp: ReturnPoint): Unit = ???

    /** transfer function for normal instructions */
    def transfer(
      inst: NormalInst,
    )(using np: NodePoint[_]): Updater = inst match {
      case ILet(id, expr) =>
        for {
          v <- transfer(expr)
          _ <- modify(_.update(id, v))
        } yield ()
      case IAssign(x: Local, expr) =>
        for {
          v <- transfer(expr)
          _ <- modify(_.update(x, v))
        } yield ()
      case _ => st => st /* simple propagation of current abstract state */
    }

    /** transfer function for expressions */
    def transfer(
      expr: Expr,
    )(using np: NodePoint[Node]): Result[AbsValue] = expr match {
      case EStr(str)                                 => AbsValue.string(str)
      case ERef(Field(Global("SYMBOL"), EStr(name))) => AbsValue.symbol(name)
      case _                                         => AbsValue.Bot
    }

    /** transfer function for unary operators */
    def transfer(
      st: AbsState,
      unary: EUnary,
      operand: AbsValue,
    )(using np: NodePoint[Node]): AbsValue = ???

    /** transfer function for binary operators */
    def transfer(
      st: AbsState,
      binary: EBinary,
      left: AbsValue,
      right: AbsValue,
    )(using np: NodePoint[Node]): AbsValue = ???

    /** transfer for variadic operators */
    def transfer(
      st: AbsState,
      vop: VOp,
      vs: List[AbsValue],
    )(using np: NodePoint[Node]): AbsValue = ???

    /** transfer for mathematical operators */
    def transfer(
      st: AbsState,
      mop: MOp,
      vs: List[AbsValue],
    )(using np: NodePoint[Node]): AbsValue = ???

    /** get next node point */
    def getNextNp(fromCp: NodePoint[Node], to: Node): NodePoint[Node] =
      fromCp.copy(node = to)
  }
}
