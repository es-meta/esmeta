package esmeta.analyzer

import esmeta.DEBUG
import esmeta.analyzer.domain.*
import esmeta.analyzer.util.*
import esmeta.cfg.*
import esmeta.error.*
import esmeta.interp.*
import esmeta.interp.util.*
import esmeta.ir.{Func => IRFunc, *}
import esmeta.js.*
import esmeta.js.util.ESValueParser
import esmeta.util.BaseUtils.*
import scala.annotation.tailrec

/** abstract transfer function */
case class AbsTransfer(sem: AbsSemantics) {

  /** loading monads */
  import AbsState.monad.*

  /** transfer function for control points */
  def apply(cp: ControlPoint): Unit = cp match
    case (np: NodePoint[_]) => this(np)
    case (rp: ReturnPoint)  => this(rp)

  /** transfer function for node points */
  def apply[T <: Node](np: NodePoint[T]): Unit = {
    val st = sem(np)
    val NodePoint(func, node, view) = np
    val helper = new Helper(np)

    import helper._
    node match {
      case Block(_, insts, next) =>
        val newSt = insts.foldLeft(st) {
          case (nextSt, inst) =>
            if (!nextSt.isBottom) transfer(inst)(nextSt)
            else nextSt
        }
        next.foreach(to => sem += getNextNp(np, to) -> newSt)
      case call: Call =>
        val (_, newSt) = (for {
          v <- transfer(call)
          _ <-
            if (v.isBottom) put(AbsState.Bot)
            else modify(_.defineLocal(call.lhs -> v))
        } yield ())(st)
        call.next.foreach(to => sem += getNextNp(np, to) -> newSt)
      case br @ Branch(_, kind, cond, thenNode, elseNode) =>
        (for {
          v <- transfer(cond)
          newSt <- get
        } yield {
          if (AVT ⊑ v)
            thenNode.foreach(to =>
              sem += getNextNp(np, to) -> prune(cond, true)(newSt),
            )
          if (AVF ⊑ v)
            elseNode.foreach(to =>
              sem += getNextNp(np, to, br.isLoop) -> prune(cond, false)(newSt),
            )
        })(st)
    }
  }

  /** get next node point */
  private def getNextNp(
    fromCp: NodePoint[Node],
    to: Node,
    loopOut: Boolean = false,
  ): NodePoint[Node] =
    val NodePoint(func, from, view) = fromCp

    // handle loop sensitivity
    val fromView = if (loopOut) sem.loopExit(view) else view
    val toView = to match
      case br: Branch if br.isLoop =>
        if (from.isLoopPred) sem.loopEnter(view, br)
        else sem.loopNext(view)
      case _ => fromView

    // next node point
    NodePoint(func, to, toView)

  /** transfer function for return points */
  def apply(rp: ReturnPoint): Unit = {
    var ret @ AbsRet(value, st) = sem(rp)

    // proper type handle
    Interp.setTypeMap
      .get(rp.func.name)
      .map(ty => {
        if (!value.unwrapCompletion.isBottom) {
          val (newV, newSt) = st.setType(value.unwrapCompletion, ty)
          value = newV ⊔ value.abruptCompletion
          st = newSt
        }
      })

    // debugging message
    if (DEBUG) println(s"<RETURN> $ret")

    // return wrapped values
    for {
      np @ NodePoint(func, call, view) <- sem.getRetEdges(rp)
      nextNode <- call.next
    } {
      val callerSt = sem.callInfo(np)
      val nextNp = NodePoint(
        func,
        nextNode,
        nextNode match {
          case br: Branch if br.isLoop => sem.loopEnter(view, br)
          case _                       => view
        },
      )

      val newSt = st.doReturn(
        callerSt,
        // wrap completion by conditions specified in
        // [5.2.3.5 Implicit Normal Completion]
        // (https://tc39.es/ecma262/#sec-implicit-normal-completion)
        call.lhs -> (if (rp.func.isReturnComp) value.wrapCompletion else value),
      )

      sem += nextNp -> newSt
    }
  }

  // transfer function for expressions
  def apply(cp: ControlPoint, expr: Expr): AbsValue = {
    val st = sem.getState(cp)
    val helper = new Helper(cp)
    helper.transfer(expr)(st)._1
  }

  /** sdo with default case */
  val defaultCases = List(
    "Contains",
    "AllPrivateIdentifiersValid",
    "ContainsArguments",
  )

  /** get syntax-directed operation(SDO) */
  private val getSDO = cached[(Ast, String), Option[(Ast, Func)]] {
    case (ast, operation) =>
      val fnameMap = cfg.fnameMap
      ast.chains.foldLeft[Option[(Ast, Func)]](None) {
        case (None, ast0) =>
          val subIdx = getSubIdx(ast0)
          val fname = s"${ast0.name}[${ast0.idx},${subIdx}].$operation"
          fnameMap.get(fname) match
            case Some(sdo) => Some(ast0, sdo)
            case None if defaultCases contains operation =>
              Some(ast0, fnameMap(s"<DEFAULT>.$operation"))
            case _ => None
        case (res: Some[_], _) => res
      }
  }

  /** get sub index of parsed Ast */
  private val getSubIdx = cached[Ast, Int] {
    case lex: Lexical => 0
    case Syntactic(name, _, rhsIdx, children) =>
      val rhs = cfg.grammar.nameMap(name).rhsList(rhsIdx)
      val optionals = (for {
        (opt, child) <- rhs.nts.map(_.optional) zip children if opt
      } yield !child.isEmpty)
      optionals.reverse.zipWithIndex.foldLeft(0) {
        case (acc, (true, idx)) => acc + scala.math.pow(2, idx).toInt
        case (acc, _)           => acc
      }
  }

  /** internal prune function */
  private trait PruneHelper { this: Helper =>

    /** prune condition */
    def prune(cond: Expr, positive: Boolean): Updater = cond match {
      case EUnary(UOp.Not, e) => prune(e, !positive)
      case EBinary(BOp.Eq, ERef(ref: Local), target) =>
        for {
          rv <- transfer(ref)
          tv <- transfer(target)
          _ <- modify(pruneValue(rv, tv, positive))
        } yield ()
      case ETypeCheck(ERef(ref: Local), tyExpr) =>
        for {
          rv <- transfer(ref)
          tv <- transfer(tyExpr)
          tname <- tv.getSingle match
            case FlatElem(ASimple(Str(s))) => pure(s)
            case FlatElem(AGrammar(n, _))  => pure(n)
            case _                         => exploded("ETypeCheck")
          _ <- modify(pruneTypeCheck(rv, tname, positive))
        } yield ()
      case EBinary(BOp.Eq, ETypeOf(ERef(ref: Local)), tyRef: ERef) =>
        for {
          rv <- transfer(ref)
          tv <- transfer(tyRef)
          _ <- modify(pruneType(rv, tv, positive))
        } yield ()
      case EBinary(BOp.Or, l, r) =>
        st =>
          val lst = prune(l, positive)(st)
          val rst = prune(r, positive)(st)
          if (positive) lst ⊔ rst else lst ⊓ rst
      case EBinary(BOp.And, l, r) =>
        st =>
          val lst = prune(l, positive)(st)
          val rst = prune(r, positive)(st)
          if (positive) lst ⊓ rst else lst ⊔ rst
      case _ => st => st
    }

    /** prune value */
    def pruneValue(l: AbsRefValue, r: AbsValue, positive: Boolean): Updater =
      for {
        lv <- transfer(l)
        st <- get
        prunedV = lv.pruneValue(r, positive)
        _ <- modify(_.update(l, prunedV))
      } yield ()

    /** prune type */
    def pruneType(l: AbsRefValue, r: AbsValue, positive: Boolean): Updater =
      for {
        lv <- transfer(l)
        st <- get
        prunedV = lv.pruneType(r, positive)
        _ <- modify(_.update(l, prunedV))
      } yield ()

    /** prune type check */
    def pruneTypeCheck(
      l: AbsRefValue,
      tname: String,
      positive: Boolean,
    ): Updater =
      for {
        lv <- transfer(l)
        st <- get
        prunedV = lv.pruneTypeCheck(tname, positive)
        _ <- modify(_.update(l, prunedV))
      } yield ()
  }

  /** internal transfer function with a specific view */
  private class Helper(val cp: ControlPoint) extends PruneHelper {
    lazy val func = cp.func
    lazy val view = cp.view
    lazy val rp = ReturnPoint(func, view)

    // record current control point for alarm
    CURRENT_CP = Some(cp)

    /** transfer function for normal instructions */
    def transfer(inst: NormalInst): Updater = inst match {
      case IExpr(expr) =>
        for {
          v <- transfer(expr)
        } yield v
      case ILet(id, expr) =>
        for {
          v <- transfer(expr)
          _ <- modify(_.defineLocal(id -> v))
        } yield ()
      case IAssign(ref, expr) =>
        for {
          rv <- transfer(ref)
          v <- transfer(expr)
          _ <- modify(_.update(rv, v))
        } yield ()
      case IDelete(ref) =>
        for {
          rv <- transfer(ref)
          _ <- modify(_.delete(rv))
        } yield ()
      case IPush(expr, list, front) =>
        for {
          l <- transfer(list)
          v <- transfer(expr)
          _ <- modify(_.push(l, v, front))
        } yield ()
      case IRemoveElem(list, elem) =>
        for {
          l <- transfer(list)
          v <- transfer(elem)
          _ <- modify(_.remove(l, v))
        } yield ()
      case IReturn(expr) =>
        for {
          v <- transfer(expr)
          _ <- doReturn(v)
          _ <- put(AbsState.Bot)
        } yield ()
      case IAssert(expr: EYet) =>
        st => st /* skip not yet compiled assertions */
      case IAssert(expr) =>
        for {
          v <- transfer(expr)
          _ <- modify(prune(expr, true))
          _ <- if (v ⊑ AVF) put(AbsState.Bot) else pure(())
        } yield sem.assertions += cp -> v
      case IPrint(expr) => st => st
      case INop()       => st => st
    }

    /** transfer function for call instructions */
    def transfer(call: Call): Result[AbsValue] =
      val callerNp = NodePoint(func, call, view)
      call.callInst match {
        case ICall(_, fexpr, args) =>
          for {
            fv <- transfer(fexpr)
            as <- join(args.map(transfer))
            st <- get
          } yield {
            // closure call
            for ((func, captured) <- fv.getClos)
              sem.doCall(callerNp, st, func, as, captured)

            // continuation call
            for (ACont(target, captured) <- fv.getConts) {
              val as0 =
                as.map(v => if (func.isReturnComp) v.wrapCompletion else v)
              val newLocals =
                getLocals(target.func, as0, cont = true) ++ captured
              sem += target -> st.copied(locals = newLocals)
            }
            AbsValue.Bot
          }
        case IMethodCall(_, base, method, args) =>
          for {
            rv <- transfer(base)
            bv <- transfer(rv)
            // TODO do not explicitly store methods in object but use a type model when
            // accessing methods
            fv <- get(_(bv, AbsValue(method)))
            as <- join(args.map(transfer))
            st <- get
          } yield {
            for ((func, _) <- fv.getClos)
              sem.doCall(callerNp, st, func, bv.refineThis(func) :: as)
            AbsValue.Bot
          }
        case ISdoCall(_, base, method, args) =>
          for {
            bv <- transfer(base)
            as <- join(args.map(transfer))
            st <- get
          } yield {
            var newV: AbsValue = AbsValue.Bot
            bv.getSingle match
              case FlatElem(AAst(syn: Syntactic)) =>
                getSDO((syn, method)) match
                  case Some((ast0, sdo)) =>
                    sem.doCall(callerNp, st, sdo, AbsValue(ast0) :: as)
                  case None => error("invalid sdo")
              case FlatElem(AAst(lex: Lexical)) =>
                newV ⊔= AbsValue(Interp.interp(lex, method))
              case FlatTop =>
                // syntactic sdo
                for { (sdo, ast) <- bv.getSDO(method) }
                  sem.doCall(callerNp, st, sdo, ast :: as)

                // lexical sdo
                newV ⊔= bv.getLexical(method)
              case _ => /* do nothing */
            newV
          }
      }

    /** transfer function for expressions */
    def transfer(expr: Expr): Result[AbsValue] = expr match {
      case EComp(ty, value, target) =>
        for {
          tyV <- transfer(ty)
          v <- transfer(value)
          targetV <- transfer(target)
        } yield AbsValue.mkCompletion(tyV, v, targetV)
      case EIsCompletion(expr) =>
        for {
          v <- transfer(expr)
        } yield v.isCompletion
      case EReturnIfAbrupt(ERef(ref), check) =>
        for {
          rv <- transfer(ref)
          v <- transfer(rv)
          newV <- returnIfAbrupt(v, check)
          _ <-
            if (!newV.isBottom) modify(_.update(rv, newV))
            else put(AbsState.Bot)
        } yield newV
      case EReturnIfAbrupt(expr, check) =>
        for {
          v <- transfer(expr)
          newV <- returnIfAbrupt(v, check)
        } yield newV
      case EPop(list, front) =>
        for {
          v <- transfer(list)
          pv <- id(_.pop(v, front))
        } yield pv
      case EParse(code, rule) =>
        for {
          c <- transfer(code)
          r <- transfer(rule)
        } yield c.parse(r)
      case EGrammar(name, params) => AbsValue(Grammar(name, params))
      case ESourceText(expr) =>
        for { v <- transfer(expr) } yield v.sourceText
      case e @ EGetChildren(kindOpt, ast) =>
        val loc = AllocSite(e.asite, cp.view)
        for {
          kOpt <- id(st => {
            kindOpt match
              case Some(kind) => transfer(kind).map(Some(_))(st)
              case None       => (None, st)
          })
          a <- transfer(ast)
          lv <- id(_.getChildren(a, kOpt, loc))
        } yield lv
      case EYet(_) => AbsValue.Bot
      case EContains(list, elem, field) =>
        for {
          l <- transfer(list)
          v <- transfer(elem)
          st <- get
        } yield st.contains(l, v, field)
      case ESubstring(expr, from, to) =>
        for {
          v <- transfer(expr)
          f <- transfer(from)
          t <- transfer(to)
        } yield v.substring(f, t)
      case ERef(ref) =>
        for {
          rv <- transfer(ref)
          v <- transfer(rv)
        } yield v
      case EUnary(uop, expr) =>
        for {
          v <- transfer(expr)
          v0 <- get(transfer(_, uop, v))
        } yield v0
      case EBinary(BOp.And, left, right) =>
        shortCircuit(BOp.And, left, right)
      case EBinary(BOp.Or, left, right) => shortCircuit(BOp.Or, left, right)
      case EBinary(BOp.Eq, ERef(ref), EAbsent) =>
        for {
          rv <- transfer(ref)
          b <- get(_.exists(rv))
        } yield !b
      case EBinary(bop, left, right) =>
        for {
          lv <- transfer(left)
          rv <- transfer(right)
          v <- get(transfer(_, bop, lv, rv))
        } yield v
      case EVariadic(vop, exprs) =>
        for {
          vs <- join(exprs.map(transfer))
        } yield transfer(vop, vs)
      case EConvert(cop, expr) =>
        import COp.*
        for {
          v <- transfer(expr)
          r <- cop match
            case ToStr(Some(radix)) => transfer(radix)
            case ToStr(None)        => pure(AbsValue(Math(10)))
            case _                  => pure(AbsValue.Bot)
        } yield v.convert(cop, r)
      case ETypeOf(base) =>
        for {
          v <- transfer(base)
          st <- get
        } yield v.typeOf(st)
      case ETypeCheck(expr, tyExpr) =>
        for {
          v <- transfer(expr)
          tv <- transfer(tyExpr)
          st <- get
          tname <- tv.getSingle match
            case FlatElem(ASimple(Str(s))) => pure(s)
            case FlatElem(AGrammar(n, _))  => pure(n)
            case _                         => exploded("ETypeCheck")
        } yield v.typeCheck(tname, st)
      case EClo(fname, cap) =>
        cfg.fnameMap.get(fname) match {
          case Some(f) =>
            for {
              st <- get
              captured = cap.map(x => x -> st.lookupLocal(x)).toMap
            } yield AbsValue(AClo(f, captured))
          case None =>
            warning(s"unknown function: $fname")
            for { _ <- put(AbsState.Bot) } yield AbsValue.Bot
        }
      case ECont(fname) =>
        for {
          st <- get
          func = cfg.fnameMap(fname)
          target = NodePoint(func, func.entry.get, cp.view)
          captured = st.locals.collect { case (x: Name, av) => x -> av }
          // return edges for resumed evaluation
          currRp = ReturnPoint(cp.func, cp.view)
          contRp = ReturnPoint(func, cp.view)
          _ = sem.retEdges += (contRp -> sem.retEdges.getOrElse(currRp, Set()))
        } yield AbsValue(ACont(target, captured))
      case ESyntactic(name, args, rhsIdx, children) =>
        for {
          cs <- join(children.map {
            case Some(child) => transfer(child).map(Some(_))
            case None        => pure(None)
          })
        } yield {
          if (cs.exists(cOpt => cOpt.fold(false)(_.isBottom))) AbsValue.Bot
          else {
            val cs0 = cs.map(cOpt =>
              cOpt.map(_.getSingle match {
                case FlatElem(AAst(child)) => child
                case FlatBot               => ??? // impossible
                case _                     => exploded("ESyntactic")
              }),
            )
            AbsValue(Syntactic(name, args, rhsIdx, cs0))
          }
        }
      case ELexical(name, expr) => ??? // TODO
      case e @ EMap(ty, props) =>
        val loc: AllocSite = AllocSite(e.asite, cp.view)
        for {
          pairs <- join(props.map {
            case (kexpr, vexpr) =>
              for {
                k <- transfer(kexpr)
                v <- transfer(vexpr)
              } yield (k, v)
          })
          lv <- id(_.allocMap(ty.name, pairs, loc))
        } yield lv
      case e @ EList(exprs) =>
        val loc: AllocSite = AllocSite(e.asite, cp.view)
        for {
          vs <- join(exprs.map(transfer))
          lv <- id(_.allocList(vs, loc))
        } yield lv
      case e @ EListConcat(exprs) =>
        val loc = AllocSite(e.asite, cp.view)
        for {
          ls <- join(exprs.map(transfer))
          lv <- id(_.listConcat(ls, loc))
        } yield lv
      case e @ ESymbol(desc) =>
        val loc = AllocSite(e.asite, cp.view)
        for {
          v <- transfer(desc)
          lv <- id(_.allocSymbol(v, loc))
        } yield lv
      case e @ ECopy(obj) =>
        val loc = AllocSite(e.asite, cp.view)
        for {
          v <- transfer(obj)
          lv <- id(_.copyObj(v, loc))
        } yield lv
      case e @ EKeys(map, intSorted) =>
        val loc = AllocSite(e.asite, cp.view)
        for {
          v <- transfer(map)
          lv <- id(_.keys(v, intSorted, loc))
        } yield lv
      case EDuplicated(expr) =>
        for {
          v <- transfer(expr)
          st <- get
        } yield v.duplicated(st)
      case EIsArrayIndex(expr) =>
        for {
          v <- transfer(expr)
        } yield v.isArrayIndex
      case EMathVal(n)           => AbsValue(Math(n))
      case ENumber(n) if n.isNaN => AbsValue(Double.NaN)
      case ENumber(n)            => AbsValue(n)
      case EBigInt(n)            => AbsValue(BigInt(n))
      case EStr(str)             => AbsValue(Str(str))
      case EBool(b)              => AbsValue(Bool(b))
      case EUndef                => AbsValue(Undef)
      case ENull                 => AbsValue(Null)
      case EAbsent               => AbsValue(Absent)
      case EConst(name)          => AbsValue(Const(name))
      case ECodeUnit(c)          => AbsValue(CodeUnit(c))
    }

    /** transfer function for references */
    def transfer(ref: Ref): Result[AbsRefValue] = ref match
      case id: Id => AbsRefId(id)
      case Prop(ref, expr) =>
        for {
          rv <- transfer(ref)
          b <- transfer(rv)
          p <- transfer(expr)
        } yield AbsRefProp(b, p)

    /** transfer function for reference values */
    def transfer(rv: AbsRefValue): Result[AbsValue] = for {
      v <- get(_(rv, cp, true))
    } yield v

    /** transfer function for unary operators */
    def transfer(
      st: AbsState,
      uop: UOp,
      operand: AbsValue,
    ): AbsValue =
      import UOp.*
      operand.getSingle match
        case FlatBot => AbsValue.Bot
        case FlatElem(ASimple(x)) =>
          optional(AbsValue(Interp.interp(uop, x))).getOrElse(AbsValue.Bot)
        case FlatElem(AMath(x)) =>
          optional(AbsValue(Interp.interp(uop, Math(x))))
            .getOrElse(AbsValue.Bot)
        case FlatElem(_) => AbsValue.Bot
        case FlatTop =>
          uop match
            case Neg   => -operand
            case Not   => !operand
            case BNot  => ~operand
            case Abs   => operand.abs
            case Floor => operand.floor

    /** transfer function for binary operators */
    def transfer(
      st: AbsState,
      bop: BOp,
      left: AbsValue,
      right: AbsValue,
    ): AbsValue =
      import BOp.*
      (left.getSingle, right.getSingle) match {
        case (FlatBot, _) | (_, FlatBot) => AbsValue.Bot
        case (FlatElem(ASimple(l)), FlatElem(ASimple(r))) =>
          optional(AbsValue(Interp.interp(bop, l, r))).getOrElse(AbsValue.Bot)
        case (FlatElem(AMath(l)), FlatElem(AMath(r))) =>
          optional(AbsValue(Interp.interp(bop, Math(l), Math(r))))
            .getOrElse(AbsValue.Bot)
        case (FlatElem(lloc: Loc), FlatElem(rloc: Loc))
            if bop == Eq || bop == Equal =>
          if (lloc == rloc) {
            if (st.isSingle(lloc)) AVT
            else AVB
          } else AVF
        case (FlatElem(l), FlatElem(r)) if bop == Eq || bop == Equal =>
          AbsValue(l == r)
        case _ =>
          bop match {
            case BAnd    => left & right
            case BOr     => left | right
            case BXOr    => left ^ right
            case Eq      => left =^= right
            case Equal   => left ==^== right
            case Lt      => left < right
            case And     => left && right
            case Or      => left || right
            case Xor     => left ^^ right
            case Plus    => left + right
            case Sub     => left sub right
            case Div     => left / right
            case Mul     => left * right
            case Mod     => left % right
            case UMod    => left %% right
            case Pow     => left ** right
            case LShift  => left << right
            case SRShift => left >> right
            case URShift => left >>> right
          }
      }

    /** transfer for variadic operators */
    def transfer(vop: VOp, vs: List[AbsValue]): AbsValue =
      AbsValue.vopTransfer(vop, vs)

    // return specific value
    def doReturn(v: AbsValue): Result[Unit] = for {
      st <- get
      ret = AbsRet(v, st.copied(locals = Map()))
      _ = sem.doReturn(rp, ret)
    } yield ()

    // return if abrupt completion
    def returnIfAbrupt(
      value: AbsValue,
      check: Boolean,
    ): Result[AbsValue] = {
      val checkReturn: Result[Unit] =
        if (check) doReturn(value.abruptCompletion)
        else ()
      for (_ <- checkReturn) yield value.unwrapCompletion
    }

    // short circuit evaluation
    def shortCircuit(
      bop: BOp,
      left: Expr,
      right: Expr,
    ): Result[AbsValue] = for {
      l <- transfer(left)
      v <- (bop, l.getSingle) match {
        case (BOp.And, FlatElem(ASimple(Bool(false)))) => pure(AVF)
        case (BOp.Or, FlatElem(ASimple(Bool(true))))   => pure(AVT)
        case _ =>
          for {
            r <- transfer(right)
            v <- get(transfer(_, bop, l, r))
          } yield v
      }
    } yield v
  }
}
