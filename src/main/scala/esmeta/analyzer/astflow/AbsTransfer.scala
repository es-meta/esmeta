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
          val newSt = transfer(call)(st)
          call.next.foreach(to => analyzer += getNextNp(np, to) -> newSt)
        case br @ Branch(_, kind, c, _, thenNode, elseNode) =>
          // TODO: handle condition `c`
          thenNode.map(analyzer += getNextNp(np, _) -> st)
          elseNode.map(analyzer += getNextNp(np, _) -> st)

    /** transfer function for return points */
    def apply(rp: ReturnPoint): Unit = {}

    /** transfer function for call instructions */
    def transfer(
      call: Call,
    )(using np: NodePoint[_]): Updater = call.callInst match {
      case ICall(lhs, _, args) =>
        for {
          vs <- join(args.map(transfer))
          st <- get
          given AbsState = st
          v = vs.foldLeft(AbsValue.Bot)(_ ⊔ _)
          _ <- modify(_.update(lhs, v))
        } yield ()
      case ISdoCall(lhs, base, _, args) =>
        for {
          b <- transfer(base)
          vs <- join(args.map(transfer))
          st <- get
          given AbsState = st
          v = vs.foldLeft(b)(_ ⊔ _)
          _ <- modify(_.update(lhs, v))
        } yield ()
    }

    /** transfer function for normal instructions */
    def transfer(
      inst: NormalInst,
    )(using np: NodePoint[_]): Updater = inst match {
      case ILet(x, expr) =>
        for {
          v <- transfer(expr)
          _ <- modify(_.update(x, v))
        } yield ()
      case IAssign(x: Global, expr) => ???
      case IAssign(x: Local, expr) =>
        for {
          v <- transfer(expr)
          _ <- modify(_.update(x, v))
        } yield ()
      case IAssign(ref, expr)       => ???
      case IExpand(base, expr)      => ???
      case IDelete(base, expr)      => ???
      case IPush(elem, list, front) => ???
      case IPop(lhs, list, front)   => ???
      case IPrint(expr)             => ???
      case INop()                   => ???
      case _ => st => st /* simple propagation of current abstract state */
    }

    /** transfer function for expressions */
    def transfer(
      expr: Expr,
    )(using np: NodePoint[Node]): Result[AbsValue] = expr match {
      case EParse(code, rule)           => ???
      case EGrammarSymbol(name, params) => ???
      case ESourceText(expr)            => ???
      case EYet(msg)                    => ???
      case EContains(list, expr)        => ???
      case ESubstring(expr, from, to)   => ???
      case ETrim(expr, isStarting)      => ???
      case ERef(ref) =>
        for {
          v <- transfer(ref)
        } yield v
      case unary @ EUnary(uop, expr) =>
        for {
          v <- transfer(expr)
          st <- get
          v0 <- transfer(st, unary, v)
        } yield v0
      case binary @ EBinary(bop, left, right) =>
        for {
          lv <- transfer(left)
          rv <- transfer(right)
          st <- get
          v <- transfer(st, binary, lv, rv)
        } yield v
      case EVariadic(vop, exprs) =>
        for {
          vs <- join(exprs.map(transfer))
          st <- get
        } yield transfer(st, vop, vs)
      case EMathOp(mop, exprs) =>
        for {
          vs <- join(exprs.map(transfer))
          st <- get
        } yield transfer(st, mop, vs)
      case EConvert(cop, expr)                      => ???
      case EExists(ref)                             => ???
      case ETypeOf(base)                            => ???
      case EInstanceOf(base, target)                => ???
      case ETypeCheck(base, ty)                     => ???
      case ESizeOf(base)                            => ???
      case EClo(fname, captured)                    => ???
      case ECont(fname)                             => ???
      case EDebug(expr)                             => ???
      case ERandom()                                => ???
      case ESyntactic(name, args, rhsIdx, children) => ???
      case ELexical(name, expr)                     => ???
      case ERecord(tname, pairs)                    => ???
      case EMap(ty, pairs)                          => ???
      case EList(exprs)                             => ???
      case ECopy(obj)                               => ???
      case EKeys(map, intSorted)                    => ???
      case _                                        => AbsValue.Bot
    }

    /** transfer function for references */
    def transfer(
      ref: Ref,
    )(using np: NodePoint[Node]): Result[AbsValue] = ref match
      case x: Global => ???
      case x: Local =>
        for {
          v <- get(_(x))
        } yield v
      case Field(base, expr) =>
        for {
          b <- transfer(base)
          f <- transfer(expr)
          st <- get
          given AbsState = st
          v = b ⊔ f
        } yield v

    /** transfer function for unary operators */
    def transfer(
      st: AbsState,
      unary: EUnary,
      operand: AbsValue,
    )(using np: NodePoint[Node]): AbsValue = operand

    /** transfer function for binary operators */
    def transfer(
      st: AbsState,
      binary: EBinary,
      left: AbsValue,
      right: AbsValue,
    )(using np: NodePoint[Node]): AbsValue =
      given AbsState = getResult(np)
      left ⊔ right

    /** transfer for variadic operators */
    def transfer(
      st: AbsState,
      vop: VOp,
      vs: List[AbsValue],
    )(using np: NodePoint[Node]): AbsValue =
      given AbsState = getResult(np)
      vs.foldLeft(AbsValue.Bot)(_ ⊔ _)

    /** transfer for mathematical operators */
    def transfer(
      st: AbsState,
      mop: MOp,
      vs: List[AbsValue],
    )(using np: NodePoint[Node]): AbsValue =
      given AbsState = getResult(np)
      vs.foldLeft(AbsValue.Bot)(_ ⊔ _)

    /** get next node point */
    def getNextNp(fromCp: NodePoint[Node], to: Node): NodePoint[Node] =
      fromCp.copy(node = to)
  }
}
