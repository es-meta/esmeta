package esmeta.analyzer.astflow

import esmeta.cfg.{util => _, *}
import esmeta.interpreter.Interpreter
import esmeta.ir.{Func => _, util => _, *}
import esmeta.state.*
import esmeta.ty.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.es.Ast

trait AbsTransferDecl { analyzer: AstFlowAnalyzer =>

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

    /** transfer function for call instructions */
    def transfer(
      call: Call,
    )(using np: NodePoint[_]): Updater = {
      call.callInst match {
        case ICall(lhs, _, args) =>
          for {
            vs <- join(args.map(transfer))
            _ <- for {
              v <- vs
            } yield modify(_.update(lhs, v))
          } yield ()
        case ISdoCall(lhs, _, _, args) =>
          for {
            vs <- join(args.map(transfer))
            _ <- for {
              v <- vs
            } yield modify(_.update(lhs, v))
          } yield ()
      }
    }

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
      case IAssign(Field(x: Local, _), expr) =>
        for {
          v <- transfer(expr)
          _ <- modify(_.update(x, v))
        } yield ()
      case IPush(expr, ERef(list: Local), _) =>
        for {
          v <- transfer(expr)
          _ <- modify(_.update(list, v))
        } yield ()
      case _ => st => st /* simple propagation of current abstract state */
    }

    /** transfer function for expressions */
    def transfer(
      expr: Expr,
    )(using np: NodePoint[Node]): Result[AbsValue] = expr match {
      // case EParse(code, rule)           => ???
      // case EGrammarSymbol(name, params) => AbsValue.ast(ESyntactic(name, params, ???, ???))
      // case ESourceText(expr) => ???
      case ERef(ref) =>
        for {
          v <- transfer(ref)
        } yield v
      // case syn: ESyntactic => AbsValue.ast(syn)
      // case lex: ELexical   => AbsValue.ast(lex)
      case _ => AbsValue.Bot
    }

    /** transfer function for references */
    def transfer(
      ref: Ref,
    )(using np: NodePoint[Node]): Result[AbsValue] = ref match
      case name: Name => AbsValue.param(Param(name))
      // case temp: Temp   => ???
      // case field: Field => ???
      case _ => AbsValue.Bot
    // case x: Var => ???
    // for {
    //   v <- get(_.get(x))
    // } yield v
    // case field @ Field(base, expr) => ???
    // for {
    //   b <- transfer(base)
    //   p <- transfer(expr)
    //   // given AbsState <- get
    //   // v <- get(_.get(b, p))
    // } yield ???

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
