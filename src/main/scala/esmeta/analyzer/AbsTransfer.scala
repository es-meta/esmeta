package esmeta.analyzer

import esmeta.analyzer.domain.*
import esmeta.analyzer.repl.*
import esmeta.analyzer.util.*
import esmeta.cfg.*
import esmeta.error.*
import esmeta.es.*
import esmeta.interpreter.Interpreter
import esmeta.ir.{Func => _, *}
import esmeta.parser.ESValueParser
import esmeta.state.*
import esmeta.ty.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import scala.annotation.tailrec

/** abstract transfer function */
trait AbsTransfer extends Optimized with PruneHelper {

  /** loading monads */
  import AbsState.monad.*

  /** fixpiont computation */
  @tailrec
  final def fixpoint: Unit = sem.worklist.next match
    case Some(cp) =>
      // set the current control point
      sem.curCp = Some(cp)
      // count how many visited for each control point
      sem.counter += cp -> (sem.getCount(cp) + 1)
      // increase iteration number
      sem.iter += 1
      // check time limit
      if (sem.iter % CHECK_PERIOD == 0) TIME_LIMIT.map(limit => {
        val duration = (System.currentTimeMillis - sem.startTime) / 1000
        if (duration > limit) exploded("timeout")
      })
      // text-based debugging
      if (DEBUG) println(s"${cp.func.name}:$cp")
      // run REPL
      if (USE_REPL) REPL(this, cp)
      // abstract transfer for the current control point
      else apply(cp)
      // keep going
      fixpoint
    case None =>
      // set the current control point
      sem.curCp = None
      // finalize REPL
      if (USE_REPL) REPL.finished

  /** transfer function for control points */
  def apply(cp: ControlPoint): Unit = cp match
    case (np: NodePoint[_]) => this(np)
    case (rp: ReturnPoint)  => this(rp)

  /** transfer function for node points */
  def apply(np: NodePoint[_]): Unit = {
    // record current control point for alarm
    given NodePoint[_] = np
    val st = sem(np)
    val NodePoint(func, node, view) = np

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
  def getNextNp(
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
    var AbsRet(value, st) = sem(rp)

    // proper type handle
    Interpreter.setTypeMap
      .get(rp.func.name)
      .map(ty => {
        if (!value.unwrapCompletion.isBottom) {
          val (newV, newSt) = st.setType(value.unwrapCompletion, ty)
          // wrap completion by conditions specified in
          // [5.2.3.5 Implicit Normal Completion]
          // (https://tc39.es/ecma262/#sec-implicit-normal-completion)
          value = if (rp.func.isReturnComp) newV.wrapCompletion else newV
          st = newSt
        }
      })

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
        call.lhs -> value,
      )

      sem += nextNp -> newSt
    }
  }

  // transfer function for expressions
  def apply(cp: ControlPoint, expr: Expr): AbsValue = {
    // record current control point for alarm
    given ControlPoint = cp
    val st = sem.getState(cp)
    transfer(expr)(st)._1
  }

  /** sdo with default case */
  val defaultCases = List(
    "Contains",
    "AllPrivateIdentifiersValid",
    "ContainsArguments",
  )

  /** get syntax-directed operation (SDO) */
  val getSDO = cached[(Ast, String), Option[(Ast, Func)]] {
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
  val getSubIdx = cached[Ast, Int] {
    case lex: Lexical => 0
    case Syntactic(name, _, rhsIdx, children) =>
      val rhs = cfg.grammar.nameMap(name).rhsList(rhsIdx)
      val optionals = (for {
        ((_, opt), child) <- rhs.ntsWithOptional zip children if opt
      } yield !child.isEmpty)
      optionals.reverse.zipWithIndex.foldLeft(0) {
        case (acc, (true, idx)) => acc + scala.math.pow(2, idx).toInt
        case (acc, _)           => acc
      }
  }

  /** transfer function for normal instructions */
  def transfer(inst: NormalInst)(using cp: NodePoint[_]): Updater = inst match {
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
    case inst @ IReturn(expr) =>
      for {
        v <- transfer(expr)
        _ <- doReturn(inst, v)
        _ <- put(AbsState.Bot)
      } yield ()
    case IAssert(expr: EYet) =>
      st => st /* skip not yet compiled assertions */
    case IAssert(expr) =>
      for {
        v <- transfer(expr)
        _ <- modify(prune(expr, true))
        _ <- if (v ⊑ AVF) put(AbsState.Bot) else pure(())
      } yield ()
    case IPrint(expr) => st => st /* skip */
    case INop()       => st => st /* skip */
  }

  /** transfer function for call instructions */
  def transfer(call: Call)(using cp: NodePoint[_]): Result[AbsValue] =
    val callerNp = NodePoint(cp.func, call, cp.view)
    call.callInst match {
      case OptimizedCall(result) => result
      case ICall(_, fexpr, args) =>
        for {
          fv <- transfer(fexpr)
          as <- join(args.map(transfer))
          st <- get
        } yield {
          // closure call (unsound for inifinitely many closures)
          for (AClo(func, captured) <- fv.clo.toIterable(stop = false))
            doCall(callerNp, st, func, as, captured)
          // continuation call (unsound for inifinitely many continuations)
          for (ACont(target, captured) <- fv.cont) {
            val as0 =
              as.map(v => if (cp.func.isReturnComp) v.wrapCompletion else v)
            val newLocals = getLocals(
              CallPoint(callerNp, target),
              as0,
              cont = true,
              method = false,
            ) ++ captured
            sem += target -> st.copied(locals = newLocals)
          }
          AbsValue.Bot
        }
      case IMethodCall(_, base, method, args) =>
        for {
          rv <- transfer(base)
          bv <- transfer(rv)
          // TODO do not explicitly store methods in object but use a type
          // model when accessing methods
          fv <- get(_.get(bv, AbsValue(Str(method))))
          as <- join(args.map(transfer))
          st <- get
        } yield {
          for (AClo(func, _) <- fv.clo)
            doCall(
              callerNp,
              st,
              func,
              bv.refineThis(func) :: as,
              method = true,
            )
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
            case One(AstValue(syn: Syntactic)) =>
              getSDO((syn, method)) match
                case Some((ast0, sdo)) =>
                  doCall(
                    callerNp,
                    st,
                    sdo,
                    AbsValue(ast0) :: as,
                    method = true,
                  )
                case None => error("invalid sdo")
            case One(AstValue(lex: Lexical)) =>
              newV ⊔= AbsValue(Interpreter.eval(lex, method))
            case Many =>
              // lexical sdo
              newV ⊔= bv.getLexical(method)

              // syntactic sdo
              for ((sdo, ast) <- bv.getSDO(method))
                doCall(callerNp, st, sdo, ast :: as, method = true)
            case _ => /* do nothing */
          newV
        }
    }

  /** transfer function for expressions */
  def transfer(expr: Expr)(using cp: ControlPoint): Result[AbsValue] =
    expr match {
      case EComp(ty, value, target) =>
        for {
          tyV <- transfer(ty)
          v <- transfer(value)
          targetV <- transfer(target)
        } yield AbsValue.createCompletion(tyV, v, targetV)
      case EIsCompletion(expr) =>
        for {
          v <- transfer(expr)
        } yield v.isCompletion
      case riaExpr @ EReturnIfAbrupt(ERef(ref), check) =>
        for {
          rv <- transfer(ref)
          v <- transfer(rv)
          newV <- returnIfAbrupt(riaExpr, v, check)
          _ <-
            if (!newV.isBottom) modify(_.update(rv, newV))
            else put(AbsState.Bot)
        } yield newV
      case riaExpr @ EReturnIfAbrupt(expr, check) =>
        for {
          v <- transfer(expr)
          newV <- returnIfAbrupt(riaExpr, v, check)
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
      case ENt(name, params) => AbsValue(Nt(name, params))
      case ESourceText(expr) =>
        for { v <- transfer(expr) } yield v.sourceText
      case e @ EGetChildren(ast) =>
        val asite = AllocSite(e.asite, cp.view)
        for {
          av <- transfer(ast)
          lv <- id(_.getChildren(asite, av))
        } yield lv
      case e @ EGetItems(nt, ast) =>
        val asite = AllocSite(e.asite, cp.view)
        for {
          nv <- transfer(nt)
          av <- transfer(ast)
          lv <- id(_.getItems(asite, nv, av))
        } yield lv
      case EYet(msg) =>
        if (YET_THROW) notSupported(msg)
        else AbsValue.Bot
      case EContains(list, elem, field) =>
        for {
          l <- transfer(list)
          v <- transfer(elem)
          st <- get
        } yield st.contains(l, v, field)
      case ESubstring(expr, from, None) =>
        for {
          v <- transfer(expr)
          f <- transfer(from)
        } yield v.substring(f)
      case ESubstring(expr, from, Some(to)) =>
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
      case EBinary(BOp.Eq, ERef(ref), EAbsent()) =>
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
      case EClamp(target, lower, upper) =>
        for {
          v <- transfer(target)
          lv <- transfer(lower)
          uv <- transfer(upper)
        } yield v.clamp(lv, uv)
      case EMathOp(mop, exprs) =>
        for {
          vs <- join(exprs.map(transfer))
        } yield transfer(mop, vs)
      case EConvert(cop, expr) =>
        import COp.*
        for {
          v <- transfer(expr)
          r <- cop match
            case ToStr(Some(radix)) => transfer(radix)
            case ToStr(None)        => pure(AbsValue(Math(10)))
            case _                  => pure(AbsValue.Bot)
        } yield v.convertTo(cop, r)
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
        } yield tv.getSingle match
          case One(Str(s))   => v.typeCheck(s, st)
          case One(Nt(n, _)) => v.typeCheck(n, st)
          case _             => AbsValue.boolTop

      case EClo(fname, cap) =>
        cfg.fnameMap.get(fname) match {
          case Some(f) =>
            for {
              st <- get
              captured = cap.map(x => x -> st.lookupLocal(x)).toMap
            } yield AbsValue(AClo(f, captured))
          case None =>
            for { _ <- put(AbsState.Bot) } yield AbsValue.Bot
        }
      case ECont(fname) =>
        for {
          st <- get
          func = cfg.fnameMap(fname)
          target = NodePoint(func, func.entry, cp.view)
          captured = st.locals.collect { case (x: Name, av) => x -> av }
          // return edges for resumed evaluation
          currRp = ReturnPoint(cp.func, cp.view)
          contRp = ReturnPoint(func, cp.view)
          _ = sem.retEdges += (contRp -> sem.retEdges.getOrElse(currRp, Set()))
        } yield AbsValue(ACont(target, captured))
      case ERandom() => pure(AbsValue.numberTop)
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
                case One(AstValue(child)) => child
                case _                    => exploded("ESyntactic")
              }),
            )
            AbsValue(Syntactic(name, args, rhsIdx, cs0))
          }
        }
      case ELexical(name, expr) => notSupported("ELexical")
      case e @ EMap(tname, props) =>
        val asite = AllocSite(e.asite, cp.view)
        for {
          pairs <- join(props.map {
            case (kexpr, vexpr) =>
              for {
                k <- transfer(kexpr)
                v <- transfer(vexpr)
              } yield (k, v)
          })
          lv <- id(_.allocMap(asite, tname, pairs))
        } yield lv
      case e @ EList(exprs) =>
        val asite = AllocSite(e.asite, cp.view)
        for {
          vs <- join(exprs.map(transfer))
          lv <- id(_.allocList(asite, vs))
        } yield lv
      case e @ EListConcat(exprs) =>
        val asite = AllocSite(e.asite, cp.view)
        for {
          ls <- join(exprs.map(transfer))
          lv <- id(_.concat(asite, ls))
        } yield lv
      case e @ ESymbol(desc) =>
        val asite = AllocSite(e.asite, cp.view)
        for {
          v <- transfer(desc)
          lv <- id(_.allocSymbol(asite, v))
        } yield lv
      case e @ ECopy(obj) =>
        val asite = AllocSite(e.asite, cp.view)
        for {
          v <- transfer(obj)
          lv <- id(_.copyObj(asite, v))
        } yield lv
      case e @ EKeys(map, intSorted) =>
        val asite = AllocSite(e.asite, cp.view)
        for {
          v <- transfer(map)
          lv <- id(_.keys(asite, v, intSorted))
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
      case EUndef()              => AbsValue(Undef)
      case ENull()               => AbsValue(Null)
      case EAbsent()             => AbsValue(Absent)
      case EConst(name)          => AbsValue(Const(name))
      case ECodeUnit(c)          => AbsValue(CodeUnit(c))
    }

  /** transfer function for references */
  def transfer(ref: Ref)(using cp: ControlPoint): Result[AbsRefValue] =
    ref match
      case id: Id => AbsRefId(id)
      case Prop(ref, expr) =>
        for {
          rv <- transfer(ref)
          b <- transfer(rv)
          p <- transfer(expr)
        } yield AbsRefProp(b, p)

  /** transfer function for reference values */
  def transfer(rv: AbsRefValue)(using cp: ControlPoint): Result[AbsValue] =
    for { v <- get(_.get(rv, cp)) } yield v

  /** transfer function for unary operators */
  def transfer(
    st: AbsState,
    uop: UOp,
    operand: AbsValue,
  )(using cp: ControlPoint): AbsValue =
    import UOp.*
    operand.getSingle match
      case Zero => AbsValue.Bot
      case One(x: SimpleValue) =>
        optional(AbsValue(Interpreter.eval(uop, x))).getOrElse(AbsValue.Bot)
      case One(Math(x)) =>
        optional(AbsValue(Interpreter.eval(uop, Math(x))))
          .getOrElse(AbsValue.Bot)
      case One(_) => AbsValue.Bot
      case Many =>
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
  )(using cp: ControlPoint): AbsValue =
    import BOp.*
    (left.getSingle, right.getSingle) match {
      case (Zero, _) | (_, Zero) => AbsValue.Bot
      case (One(l: SimpleValue), One(r: SimpleValue)) =>
        optional(AbsValue(Interpreter.eval(bop, l, r)))
          .getOrElse(AbsValue.Bot)
      case (One(Math(l)), One(Math(r))) =>
        optional(AbsValue(Interpreter.eval(bop, Math(l), Math(r))))
          .getOrElse(AbsValue.Bot)
      case (One(lpart: Part), One(rpart: Part)) if bop == Eq || bop == Equal =>
        if (lpart == rpart) {
          if (st.isSingle(lpart)) AVT
          else AVB
        } else AVF
      case (One(l), One(r)) if bop == Eq || bop == Equal =>
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
          case Add     => left + right
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
  def transfer(vop: VOp, vs: List[AbsValue])(using cp: ControlPoint): AbsValue =
    AbsValue.vopTransfer(vop, vs)

  /** transfer for mathematical operators */
  def transfer(mop: MOp, vs: List[AbsValue])(using cp: ControlPoint): AbsValue =
    AbsValue.mopTransfer(mop, vs)

  /** handle calls */
  def doCall(
    callerNp: NodePoint[Call],
    callerSt: AbsState,
    calleeFunc: Func,
    args: List[AbsValue],
    captured: Map[Name, AbsValue] = Map(),
    method: Boolean = false,
  ): Unit =
    sem.callInfo += callerNp -> callerSt
    for {
      (calleeNp, calleeSt) <- getCalleeEntries(
        callerNp,
        callerSt,
        calleeFunc,
        args,
        captured,
        method,
      )
    } {
      // add callee to worklist
      sem += calleeNp -> calleeSt.doCall
      // add return edges from callee to caller
      val rp = ReturnPoint(calleeFunc, calleeNp.view)
      val set = sem.getRetEdges(rp)
      sem.retEdges += rp -> (set + callerNp)
      // propagate callee analysis result
      val retT = sem(rp)
      if (!retT.isBottom) sem.worklist += rp
    }

  // return specific value
  def doReturn(
    irReturn: Return,
    v: AbsValue,
  )(using cp: ControlPoint): Result[Unit] = for {
    st <- get
    ret = AbsRet(v, st.copied(locals = Map()))
    calleeRp = ReturnPoint(cp.func, cp.view)
    irp = InternalReturnPoint(irReturn, calleeRp)
    _ = doReturn(irp, ret)
  } yield ()

  /** update return points */
  def doReturn(irp: InternalReturnPoint, givenRet: AbsRet): Unit =
    val InternalReturnPoint(_, ReturnPoint(func, view)) = irp
    val retRp = ReturnPoint(func, sem.getEntryView(view))
    // wrap completion by conditions specified in
    // [5.2.3.5 Implicit Normal Completion]
    // (https://tc39.es/ecma262/#sec-implicit-normal-completion)
    val newRet = if (func.isReturnComp) givenRet.wrapCompletion else givenRet
    if (!newRet.value.isBottom)
      val oldRet = sem(retRp)
      if (!oldRet.isBottom && USE_REPL) REPL.merged = true
      if (newRet !⊑ oldRet)
        sem.rpMap += retRp -> (oldRet ⊔ newRet)
        sem.worklist += retRp

  /** return-if-abrupt completion */
  def returnIfAbrupt(
    riaExpr: EReturnIfAbrupt,
    value: AbsValue,
    check: Boolean,
  )(using cp: ControlPoint): Result[AbsValue] = {
    val checkReturn: Result[Unit] =
      if (check) doReturn(riaExpr, value.abruptCompletion)
      else ()
    for (_ <- checkReturn) yield value.unwrapCompletion
  }

  // short circuit evaluation
  def shortCircuit(
    bop: BOp,
    left: Expr,
    right: Expr,
  )(using cp: ControlPoint): Result[AbsValue] = for {
    l <- transfer(left)
    v <- (bop, l.getSingle) match {
      case (BOp.And, One(Bool(false))) => pure(AVF)
      case (BOp.Or, One(Bool(true)))   => pure(AVT)
      case _ =>
        for {
          r <- transfer(right)
          v <- get(transfer(_, bop, l, r))
        } yield v
    }
  } yield v

  /** call transition */
  def getCalleeEntries(
    callerNp: NodePoint[Call],
    callerSt: AbsState,
    calleeFunc: Func,
    args: List[AbsValue],
    captured: Map[Name, AbsValue],
    method: Boolean,
  ): List[(NodePoint[_], AbsState)] = {
    // handle ir callsite sensitivity
    val NodePoint(callerFunc, callSite, callerView) = callerNp
    val baseView =
      if (IR_SENS)
        callerView.copy(
          calls = callSite :: callerView.calls,
          intraLoopDepth = 0,
        )
      else callerView

    val calleeNp = NodePoint(calleeFunc, calleeFunc.entry, baseView)
    val calleeSt = callerSt.copied(locals =
      getLocals(
        CallPoint(callerNp, calleeNp),
        args,
        cont = false,
        method,
      ) ++ captured,
    )
    List((calleeNp, calleeSt))
  }

  /** get local variables */
  def getLocals(
    cp: CallPoint[Node],
    args: List[AbsValue],
    cont: Boolean,
    method: Boolean,
  ): Map[Local, AbsValue] = {
    val CallPoint(callerNp, calleeNp) = cp
    val params: List[Param] = calleeNp.func.irFunc.params
    var map = Map[Local, AbsValue]()

    @tailrec
    def aux(ps: List[Param], as: List[AbsValue]): Unit = (ps, as) match {
      case (Nil, Nil) =>
      case (Param(lhs, _, optional, _) :: pl, Nil) =>
        if (optional) {
          map += lhs -> AbsValue(Absent)
          aux(pl, Nil)
        }
      case (Nil, args) =>
      // XXX Handle GeneratorStart <-> GeneratorResume arith mismatch
      case (param :: pl, arg :: al) =>
        map += param.lhs -> arg
        aux(pl, al)
    }
    aux(params, args)
    map
  }
}
