package esmeta.analyzer.paramflow

import esmeta.cfg.{util => _, *}
import esmeta.ir.{Func => _, util => _, *}

trait AbsTransferDecl { analyzer: ParamFlowAnalyzer =>

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
        case br @ Branch(_, kind, cond, _, thenNode, elseNode) =>
          // TODO: handle condition `cond`
          // TODO: support context (e.g. Object.prototype.toString step 4)
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
      case IAssign(x: Local, expr) =>
        for {
          v <- transfer(expr)
          _ <- modify(_.update(x, v))
        } yield ()
      case IAssign(Field(x: Local, EStr(f)), expr) =>
        for {
          v <- transfer(expr)
          _ <- modify(_.update(x, v))
        } yield ()
      case IExpand(base: Local, EStr(f)) =>
        for {
          _ <- modify(_.update(base, AbsValue(f)))
        } yield ()
      case IDelete(base, expr) =>
        st => st
      case IPush(expr, ERef(list: Local), _) =>
        for {
          l <- transfer(list)
          v <- transfer(expr)
          st <- get
          given AbsState = st
          newV = l ⊔ v
          _ <- modify(_.update(list, newV))
        } yield ()
      case IPop(lhs: Name, ERef(Name("ArgumentsList")), _) =>
        // FIXME: ad-hoc impl. to handle BuiltinPrefix (ArgumentsList)
        for {
          _ <- modify(_.update(lhs, AbsValue(lhs.name)))
        } yield ()
      case IPop(lhs, list, _) =>
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
          c <- transfer(code)
          r <- transfer(rule)
          st <- get
          given AbsState = st
          v = c ⊔ r
        } yield v
      case ESourceText(expr) =>
        for {
          v <- transfer(expr)
        } yield v
      case EContains(list, _) =>
        for {
          v <- transfer(list)
        } yield v
      case ESubstring(expr, from, None) =>
        for {
          v <- transfer(expr)
          f <- transfer(from)
          st <- get
          given AbsState = st
          v0 = v ⊔ f
        } yield v0
      case ESubstring(expr, from, Some(to)) =>
        for {
          v <- transfer(expr)
          f <- transfer(from)
          t <- transfer(to)
          st <- get
          given AbsState = st
          v0 = v ⊔ f ⊔ t
        } yield v0
      case ETrim(expr, _) =>
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
      case EConvert(_, expr) =>
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
      case EInstanceOf(base, _) =>
        for {
          v <- transfer(base)
        } yield v
      case ETypeCheck(base, _) =>
        for {
          v <- transfer(base)
        } yield v
      case ESizeOf(base) =>
        for {
          v <- transfer(base)
        } yield v
      case EClo(_, captured) =>
        for {
          vs <- join(captured.map(transfer))
          st <- get
          given AbsState = st
          v = vs.foldLeft(AbsValue.Bot)(_ ⊔ _)
        } yield v
      case ESyntactic(_, _, _, children) =>
        for {
          vs <- join(children.flatten.map(transfer))
          st <- get
          given AbsState = st
          v = vs.foldLeft(AbsValue.Bot)(_ ⊔ _)
        } yield v
      case ELexical(_, expr) =>
        for {
          v <- transfer(expr)
        } yield v
      case ERecord(_, pairs) =>
        for {
          vs <- join(pairs.map(_._2).map(transfer))
          st <- get
          given AbsState = st
          v = vs.foldLeft(AbsValue.Bot)(_ ⊔ _)
        } yield v
      case EMap(_, pairs) =>
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
      case EKeys(map, _) =>
        for {
          v <- transfer(map)
        } yield v
      case _ => AbsValue.Bot
    }

    /** transfer function for references */
    def transfer(
      ref: Ref,
    )(using np: NodePoint[Node]): Result[AbsValue] = ref match
      case x: Global => AbsValue.Bot
      case x: Local =>
        for {
          v <- get(_(x))
        } yield v
      case field @ Field(base, expr) if field.baseName == "this" =>
        // FIXME: ad-hoc impl. to handle precise this
        for {
          f <- transfer(expr)
          st <- get
          given AbsState = st
          v = AbsValue(field.toString) ⊔ f
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
