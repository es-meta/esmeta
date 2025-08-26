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
      case IAssign(x: Global, expr) => st => st /* TODO */
      case IAssign(x: Local, expr) =>
        for {
          v <- transfer(expr)
          _ <- modify(_.update(x, v))
        } yield ()
      case IAssign(ref, expr)          => st => st /* TODO */
      case IExpand(base: Global, expr) => st => st /* TODO */
      case IExpand(base: Local, expr) =>
        for {
          bv <- transfer(base)
          v <- transfer(expr)
          st <- get
          given AbsState = st
          newV = bv ⊔ v
          _ <- modify(_.update(base, newV))
        } yield ()
      case IExpand(ref, expr)  => st => st /* TODO */
      case IDelete(base, expr) => st => st /* TODO */
      case IPush(expr, ERef(list: Local), _) =>
        for {
          l <- transfer(list)
          v <- transfer(expr)
          st <- get
          given AbsState = st
          newV = l ⊔ v
          _ <- modify(_.update(list, newV))
        } yield ()
      case IPush(expr, list, front) => st => st /* TODO */
      case IPop(lhs, list, front) =>
        for {
          v <- transfer(list)
          _ <- modify(_.update(lhs, v))
        } yield ()
      case _ => st => st /* simple propagation of current abstract state */
    }

    /** transfer function for expressions */
    def transfer(
      expr: Expr,
    )(using np: NodePoint[Node]): Result[AbsValue] = expr match {
      case EParse(code, rule) =>
        for {
          ce <- transfer(code)
          re <- transfer(rule)
          st <- get
          given AbsState = st
          v = ce ⊔ re
        } yield v
      case EGrammarSymbol(name, params) =>
        AbsValue(
          s"|$name|[${params.map(b => if (b) then "T" else "F").mkString}]",
        )
      case ESourceText(expr) =>
        for {
          v <- transfer(expr)
        } yield v
      case EContains(list, expr) =>
        for {
          v <- transfer(list)
        } yield v
      case ESubstring(expr, from, to) =>
        for {
          v <- transfer(expr)
        } yield v
      case ETrim(expr, isStarting) =>
        for {
          v <- transfer(expr)
        } yield v
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
      case EConvert(cop, expr) =>
        for {
          v <- transfer(expr)
        } yield v
      case EExists(ref) =>
        for {
          v <- transfer(ref)
        } yield v
      case ETypeOf(base) =>
        for {
          v <- transfer(base)
        } yield v
      case EInstanceOf(base, target) =>
        for {
          v <- transfer(base)
        } yield v
      case ETypeCheck(base, ty) =>
        for {
          v <- transfer(base)
        } yield v
      case ESizeOf(base) =>
        for {
          v <- transfer(base)
        } yield v
      case EClo(fname, captured) =>
        for {
          vs <- join(captured.map(transfer))
          st <- get
          given AbsState = st
          v = vs.foldLeft(AbsValue.Bot)(_ ⊔ _)
        } yield v
      case EDebug(expr) =>
        for {
          v <- transfer(expr)
        } yield v
      case ESyntactic(name, args, rhsIdx, children) => // FIXME: temporary
        AbsValue(
          s"|$name|[${args.map(b => if (b) then "T" else "F").mkString}]<$rhsIdx>",
        )
      case ELexical(name, expr) =>
        AbsValue(s"|$name|($expr)") // FIXME: temporary
      case ERecord(tname, pairs) =>
        for {
          vs <- join(pairs.map(_._2).map(transfer))
          st <- get
          given AbsState = st
          v = vs.foldLeft(AbsValue.Bot)(_ ⊔ _)
        } yield v
      case EMap(ty, pairs) =>
        for {
          ks <- join(pairs.map(_._1).map(transfer))
          vs <- join(pairs.map(_._2).map(transfer))
          st <- get
          given AbsState = st
          v = (ks ++ vs).foldLeft(AbsValue.Bot)(_ ⊔ _)
        } yield v
      case EList(exprs) =>
        for {
          vs <- join(exprs.map(transfer))
          st <- get
          given AbsState = st
          v = vs.foldLeft(AbsValue.Bot)(_ ⊔ _)
        } yield v
      case ECopy(obj) =>
        for {
          v <- transfer(obj)
        } yield v
      case EKeys(map, intSorted) =>
        for {
          v <- transfer(map)
        } yield v
      case _ => AbsValue.Bot
    }

    /** transfer function for references */
    def transfer(
      ref: Ref,
    )(using np: NodePoint[Node]): Result[AbsValue] = ref match
      case x: Global => AbsValue.Bot // FIXME: temporary
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
