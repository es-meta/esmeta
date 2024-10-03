package esmeta.analyzer.tychecker

import esmeta.cfg.{util => _, *}
import esmeta.ir.{Func => _, util => _, *}
import esmeta.state.*
import esmeta.ty.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import scala.annotation.tailrec

trait AbsTransferDecl { analyzer: TyChecker =>

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
      // record current control point for alarm
      val st = getResult(np)
      given NodePoint[_] = np
      given AbsState = st
      val NodePoint(func, node, view) = np
      node match
        case Block(_, insts, next) =>
          val newSt = insts.foldLeft(st) {
            case (nextSt, _) if nextSt.isBottom => nextSt
            case (nextSt, inst)                 => transfer(inst)(nextSt)
          }
          next.foreach(to => analyzer += getNextNp(np, to) -> newSt)
        case call: Call =>
          val (_, newSt) = (for {
            v <- transfer(call)
            _ <-
              if (v.isBottom) put(AbsState.Bot)
              else modify(_.define(call.lhs, v))
          } yield ())(st)
          call.next.foreach(to => analyzer += getNextNp(np, to) -> newSt)
        case br @ Branch(_, kind, c, thenNode, elseNode) =>
          import RefinementKind.*
          (for { v <- transfer(c); newSt <- get } yield {
            if (v.ty.bool.contains(true))
              val refinedSt = refine(c, v, true)(newSt)
              thenNode.map(analyzer += getNextNp(np, _) -> refinedSt)
            if (v.ty.bool.contains(false))
              val refinedSt = refine(c, v, false)(newSt)
              elseNode.map(analyzer += getNextNp(np, _) -> refinedSt)
          })(st)

    def refine(
      expr: Expr,
      v: AbsValue,
      positive: Boolean,
    )(using np: NodePoint[_]): Updater = st =>
      import RefinementKind.*, SymExpr.*
      val kind = if (positive) True else False
      val newSt = (for {
        ref <- toSymRef(expr, v)
        if useTypeGuard
      } yield refine(SymPred(Map(ref -> TrueT)), positive)(st))
        .getOrElse(st)
      v.guard.get(kind) match
        case Some(pred) => refine(pred, true)(newSt)
        case None => if (useTypeGuard) newSt else refine(expr, positive)(st)

    /** get next node point */
    def getNextNp(fromCp: NodePoint[Node], to: Node): NodePoint[Node] =
      fromCp.copy(node = to)

    /** transfer function for return points */
    def apply(rp: ReturnPoint): Unit = if (!canUseReturnTy(rp.func)) {
      var AbsRet(value) = getResult(rp)
      for {
        callerNps <- retEdges.get(rp)
        callerNp <- callerNps
        nextNp <- getAfterCallNp(callerNp)
      } {
        val callerSt = callInfo(callerNp)
        analyzer += nextNp -> callerSt.update(
          callerNp.node.lhs,
          value,
          refine = false,
        )
      }
    }

    /** get after call node point */
    def getAfterCallNp(callerNp: NodePoint[Call]): Option[NodePoint[Node]] =
      callerNp.node.next.map(nextNode => callerNp.copy(node = nextNode))

    /** transfer function for call instructions */
    def transfer(
      call: Call,
    )(using np: NodePoint[_]): Result[AbsValue] = {
      val callerNp = NodePoint(np.func, call, np.view)
      call.callInst match {
        case ICall(_, fexpr, args) =>
          for {
            fv <- transfer(fexpr)
            st <- get
            given AbsState = st
            fty = fv.ty
            vs <- join(args.map(transfer))
          } yield {
            // closure call (XXX: unsound for inifinitely many closures)
            for {
              fname <- fty.clo.toIterable(stop = false)
              f <- cfg.fnameMap.get(fname)
            } {
              val callPoint = CallPoint(callerNp, f)
              val captured: Map[Name, AbsValue] = Map() // TODO
              doCall(callPoint, st, args, vs, captured, f.isMethod)
            }
            // continuation call (XXX: unsound for inifinitely many continuations)
            for {
              fid <- fty.cont.toIterable(stop = false)
              f <- cfg.funcMap.get(fid)
              tgt = NodePoint(f, f.entry, emptyView)
            } {
              val callPoint = CallPoint(callerNp, f)
              val captured: Map[Name, AbsValue] = Map() // TODO
              doCall(callPoint, st, args, vs, captured, f.isMethod, Some(tgt))
            }
            if (fty.clo.isTop || fty.cont.isTop) Top
            else Bot
          }
        case ISdoCall(_, base, method, args) =>
          for {
            bv <- transfer(base)
            vs <- join(args.map(transfer))
            st <- get
            given AbsState = st
            bty = bv.ty
          } yield {
            var newV: AbsValue = Bot
            // lexical sdo
            newV ⊔= bv.getLexical(method)

            // syntactic sdo
            for ((sdo, ast) <- bv.getSdo(method))
              val callPoint = CallPoint(callerNp, sdo)
              doCall(callPoint, st, args, ast :: vs, method = true)

            newV
          }
      }
    }

    /** handle calls */
    def doCall(
      callPoint: CallPoint,
      callerSt: AbsState,
      args: List[Expr],
      vs: List[AbsValue],
      captured: Map[Name, AbsValue] = Map(),
      method: Boolean = false,
      contTarget: Option[NodePoint[Node]] = None,
    ): Unit = {
      given AbsState = callerSt
      val CallPoint(callerNp, callee) = callPoint
      if (canUseReturnTy(callee)) {
        val call = callerNp.node
        val retTy = callee.retTy.ty.toValue
        val map = (args zip vs)
          .map(toSymRef)
          .zipWithIndex
          .collect { case (Some(ref), i) => i -> ref }
          .toMap
        val newRetV = (for {
          refine <- typeGuards.get(callee.name)
          v = refine(vs, retTy, callerSt)
          guard = for {
            (kind, pred) <- v.guard
            newPred = instantiate(pred, map)
          } yield kind -> newPred
          newV = AbsValue(v.ty, Zero, guard)
        } yield newV).getOrElse(AbsValue(retTy))
        for {
          nextNp <- getAfterCallNp(callerNp)
          newSt = callerSt.define(call.lhs, newRetV)
        } analyzer += nextNp -> newSt
      }
      // get locals
      val locals = getLocals(callPoint, method, vs) ++ captured
      // keep caller state to restore it
      contTarget match
        case Some(target) =>
          analyzer += target -> getCalleeState(callerSt, locals)
        case None =>
          callInfo += callerNp -> callerSt
          for {
            (calleeView, newLocals) <- getCalleeEntries(callerNp, locals)
            calleeSt = getCalleeState(callerSt, newLocals)
            calleeNp = NodePoint(callee, callee.entry, calleeView)
          } {
            // add callee to worklist
            analyzer += calleeNp -> calleeSt
            // add return edges from callee to caller
            val rp = ReturnPoint(callee, calleeNp.view)
            val set = retEdges.getOrElse(rp, Set())
            retEdges += rp -> (set + callerNp)
            // propagate callee analysis result
            propagate(rp, callerNp)
          }
    }

    /** conversion to symbolic bases */
    def toSymRef(pair: (Expr, AbsValue)): Option[SymRef] =
      val (expr, value) = pair
      toSymRef(expr, value)

    /** conversion to symbolic references */
    def toSymRef(expr: Expr, value: AbsValue): Option[SymRef] = expr match
      case ERef(ref) => toSymRef(ref, value)
      case _         => None

    /** conversion to symbolic references */
    def toSymRef(ref: Ref, value: AbsValue): Option[SymRef] =
      import SymExpr.*, SymRef.*
      value.expr match
        case One(SERef(ref)) => Some(ref)
        case _               => toSymRef(ref)

    /** conversion to symbolic references */
    def toSymRef(ref: Ref): Option[SymRef] =
      import SymExpr.*, SymRef.*
      ref match
        case s: Local => Some(SLocal(s))
        case Field(base, EStr(field)) =>
          for {
            b <- toSymRef(base)
          } yield SField(b, SEStr(field))
        case _ => None

    /** get local variables */
    def getLocals(
      callPoint: CallPoint,
      method: Boolean,
      vs: List[AbsValue],
    ): List[(Local, AbsValue)] = {
      val CallPoint(callerNp, callee) = callPoint
      val arity @ (from, to) = callee.arity
      val len = vs.length
      if (config.checkArity && (len < from || to < len))
        addError(ArityMismatch(callPoint, len))
      // get parameters
      val params: List[Param] = callee.irFunc.params
      // full arguments with optional parameters
      // construct local type environment
      (for {
        ((param, arg), idx) <- (params zip vs).zipWithIndex
      } yield param.lhs -> assignArg(callPoint, method, idx, param, arg))
    }

    /** assign argument to parameter */
    def assignArg(
      callPoint: CallPoint,
      method: Boolean,
      idx: Int,
      param: Param,
      arg: AbsValue,
    ): AbsValue =
      given AbsState = getResult(callPoint.callerNp)
      val paramTy = param.ty.ty.toValue
      val argTy = arg.ty
      if (method && idx == 0) () /* ignore `this` for method calls */
      else if (config.checkParamType && !(argTy <= paramTy))
        addError(ParamTypeMismatch(ArgAssignPoint(callPoint, idx), argTy))
      AbsValue(paramTy && argTy)

    /** callee entries */
    def getCalleeEntries(
      callerNp: NodePoint[Call],
      locals: List[(Local, AbsValue)],
    ): List[(View, List[(Local, AbsValue)])] = {
      given AbsState = getResult(callerNp)
      List(emptyView -> (for {
        (local, value) <- locals
      } yield local -> AbsValue(value.ty)))
    }

    /** propagate callee analysis result */
    def propagate(rp: ReturnPoint, callerNp: NodePoint[Call]): Unit = {
      if (!canUseReturnTy(rp.func)) {
        val AbsRet(value) = getResult(rp)
        (for {
          nextNp <- getAfterCallNp(callerNp)
          if !value.isBottom
          callerSt = callInfo(callerNp)
        } yield analyzer += nextNp -> callerSt.define(callerNp.node.lhs, value))
          .getOrElse {
            if (!getResult(rp).isBottom) worklist += rp
          }
      }
    }

    /** transfer function for normal instructions */
    def transfer(
      inst: NormalInst,
    )(using np: NodePoint[_]): Updater = inst match {
      case IExpr(expr) =>
        for {
          v <- transfer(expr)
          _ <- if (v.isBottom) put(AbsState.Bot) else pure(())
        } yield ()
      case ILet(id, expr) =>
        for {
          v <- transfer(expr)
          _ <- modify(_.define(id, v))
          st <- get
        } yield ()
      case IAssign(x: Local, expr) =>
        for {
          v <- transfer(expr)
          _ <- modify(_.update(x, v, refine = false))
        } yield ()
      case IAssign(Field(x: Var, EStr(f)), expr) =>
        for {
          v <- transfer(expr)
          given AbsState <- get
          ty <- get(_.get(x).ty)
          record = ty.record.update(f, v.ty, refine = false)
          _ <- modify(
            _.update(x, AbsValue(ty.copied(record = record)), refine = false),
          )
        } yield ()
      case IAssign(ref, expr)       => st => st /* TODO */
      case IExpand(base, expr)      => st => st /* TODO */
      case IDelete(base, expr)      => st => st /* TODO */
      case IPush(expr, list, front) => st => st /* TODO */
      case IPop(lhs, list, front) =>
        for {
          v <- transfer(list)
          pv <- id(_.pop(v, front))
          _ <- modify(_.define(lhs, pv))
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
          _ <- modify(refine(expr, v, true))
          given AbsState <- get
          _ <- if (v ⊑ False) put(AbsState.Bot) else pure(())
        } yield ()
      case IPrint(expr) => st => st /* skip */
      case INop()       => st => st /* skip */
    }

    // return specific value
    def doReturn(
      irReturn: Return,
      v: AbsValue,
    )(using returnNp: NodePoint[Node]): Result[Unit] = for {
      st <- get
      ret = AbsRet(v)
      irp = InternalReturnPoint(returnNp, irReturn)
      _ = doReturn(irp, ret)
    } yield ()

    /** update return points */
    def doReturn(
      irp: InternalReturnPoint,
      givenRet: AbsRet,
    ): Unit =
      val InternalReturnPoint(NodePoint(func, _, view), irReturn) = irp
      given AbsState = givenRet.state
      val givenTy = givenRet.value.ty
      val newRet = func.retTy.ty match
        case _: UnknownTy        => givenRet
        case expectedTy: ValueTy =>
          // return type check when it is a known type
          if (config.checkReturnType && !(givenTy <= expectedTy))
            addError(ReturnTypeMismatch(irp, givenTy))
          AbsRet(AbsValue(givenTy && expectedTy))
      val retRp = ReturnPoint(func, getEntryView(view))
      if (!newRet.value.isBottom)
        val oldRet = getResult(retRp)
        if (!oldRet.isBottom && useRepl) Repl.merged = true
        if (newRet !⊑ oldRet)
          rpMap += retRp -> (oldRet ⊔ newRet)
          worklist += retRp

    /** transfer function for expressions */
    def transfer(
      expr: Expr,
    )(using np: NodePoint[Node]): Result[AbsValue] = expr match {
      case Refiner(v) => v
      case EParse(code, rule) =>
        for {
          c <- transfer(code)
          r <- transfer(rule)
          given AbsState <- get
        } yield c.parse(r)
      case EGrammarSymbol(name, params) =>
        val s = GrammarSymbol(name, params)
        AbsValue(GrammarSymbolT(s))
      case ESourceText(expr) =>
        for {
          v <- transfer(expr)
        } yield StrTop
      case EYet(msg) =>
        if (yetThrow) notSupported(msg)
        else Bot
      case EContains(list, elem) =>
        for {
          l <- transfer(list)
          v <- transfer(elem)
          given AbsState <- get
        } yield
          if (l.ty.list.isBottom) Bot
          else BoolTop
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
      case ETrim(expr, isStarting) =>
        for {
          v <- transfer(expr)
        } yield v.trim(isStarting)
      // a precise type of `the active function object` in built-in functions
      case ERef(
            Field(
              Field(Global("EXECUTION_STACK"), EMath(0)),
              EStr("Function"),
            ),
          ) if np.func.isBuiltin =>
        AbsValue(RecordT("Constructor"))
      // a precise type for intrinsic objects
      case ERef(Field(Field(base, EStr("Intrinsics")), EStr(name))) =>
        for {
          b <- transfer(base)
          given AbsState <- get
          v <-
            if (b.ty <= RealmT) {
              pure(AbsValue(cfg.init.intr.kinds.getOrElse(name, ObjectT)))
            } else transfer(base)
        } yield v
      case ERef(ref) =>
        for {
          v <- transfer(ref)
        } yield v
      case unary @ EUnary(_, expr) =>
        for {
          v <- transfer(expr)
          v0 <- transfer(unary, v)
        } yield v0
      case binary @ EBinary(_, left, right) =>
        for {
          lv <- transfer(left)
          rv <- transfer(right)
          v <- transfer(binary, lv, rv)
        } yield v
      case EVariadic(vop, exprs) =>
        for {
          vs <- join(exprs.map(transfer))
        } yield transfer(vop, vs)
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
            case ToStr(None)        => pure(AbsValue(MathT(10)))
            case _                  => pure(Bot)
          given AbsState <- get
        } yield v.convertTo(cop, r)
      case EExists(ref) =>
        for {
          v <- get(_.exists(ref))
        } yield v
      case ETypeOf(base) =>
        for {
          v <- transfer(base)
          given AbsState <- get
        } yield v.typeOf
      case EInstanceOf(expr, target) =>
        for {
          v <- transfer(expr)
          t <- transfer(target)
        } yield v.instanceOf(t)
      case ETypeCheck(expr, ty) =>
        for {
          v <- transfer(expr)
        } yield BoolTop
      case ESizeOf(expr) =>
        for {
          v <- transfer(expr)
          given AbsState <- get
        } yield v.sizeOf
      case EClo(fname, cap) => AbsValue(CloT(fname))
      case ECont(fname)     => AbsValue(ContT(cfg.fnameMap(fname).id))
      case EDebug(expr) =>
        for {
          v <- transfer(expr)
          st <- get
          _ = debug(s"[[ $expr @ $np ]]($st) = $v")
        } yield v
      case ERandom() => pure(NumberTop)
      case ESyntactic(name, _, rhsIdx, _) =>
        pure(AbsValue(AstT(name, rhsIdx)))
      case ELexical(name, expr) =>
        ???
      case ERecord(tname, fields) =>
        for {
          pairs <- join(fields.map {
            case (f, expr) =>
              for {
                v <- transfer(expr)
              } yield (f, v)
          })
          lv <- id(_.allocRecord(tname, pairs))
        } yield lv
      case EMap((kty, vty), _) => AbsValue(MapT(kty.toValue, vty.toValue))
      case EList(exprs) =>
        for {
          vs <- join(exprs.map(transfer))
          lv <- id(_.allocList(vs))
        } yield lv
      case ECopy(obj) =>
        for {
          v <- transfer(obj)
          lv <- id(_.copy(v))
        } yield lv
      case EKeys(map, intSorted) =>
        for {
          v <- transfer(map)
          lv <- id(_.keys(v, intSorted))
        } yield lv
      case EMath(n)              => AbsValue(MathT(n))
      case EInfinity(pos)        => AbsValue(InfinityT(pos))
      case ENumber(n) if n.isNaN => AbsValue(NumberT(Number(Double.NaN)))
      case ENumber(n)            => AbsValue(NumberT(Number(n)))
      case EBigInt(n)            => AbsValue(BigIntT)
      case EStr(str)             => AbsValue(StrT(str))
      case EBool(b)              => AbsValue(BoolT(b))
      case EUndef()              => AbsValue(UndefT)
      case ENull()               => AbsValue(NullT)
      case EEnum(name)           => AbsValue(EnumT(name))
      case ECodeUnit(c)          => AbsValue(CodeUnitT)
    }

    object Refiner {
      import RefinementKind.*, SymExpr.*, SymRef.*
      def apply(
        expr: Expr,
      )(using np: NodePoint[_]): Option[Result[AbsValue]] = unapply(expr)

      def unapply(
        expr: Expr,
      )(using np: NodePoint[_]): Option[Result[AbsValue]] = {
        def withPred(pred: SymPred)(using st: AbsState): SymPred =
          // TODO st.pred.fold(pred)(_ && pred)
          pred
        def refineField(
          x: Local,
          field: String,
          binding: Binding,
        ): Result[AbsValue] = for {
          lv <- transfer(x)
          given AbsState <- get
        } yield {
          val lty = lv.ty
          def aux(binding: Binding) = ValueTy(
            ast = lty.ast,
            record = lty.record.update(field, binding, refine = true),
          )
          val thenTy = aux(binding)
          val elseTy = aux(lty.record(field) -- binding)
          var guard: TypeGuard = Map()
          toSymRef(x, lv).map { ref =>
            if (lty != thenTy)
              guard += True -> withPred(SymPred(Map(ref -> thenTy)))
            if (lty != elseTy)
              guard += False -> withPred(SymPred(Map(ref -> elseTy)))
          }
          AbsValue(BoolT, Many, guard)
        }
        if (!useTypeGuard) None
        else
          expr match {
            case EBinary(BOp.Lt, l, r) =>
              Some(for {
                lv <- transfer(l)
                rv <- transfer(r)
                given AbsState <- get
              } yield {
                val lty = lv.ty
                val rty = rv.ty
                val lmath = lty.math
                val rmath = rty.math
                def aux(
                  lty: ValueTy,
                  rty: ValueTy,
                  pos: Boolean,
                  isLt: Boolean,
                ): Option[ValueTy] = {
                  var math = lty.math
                  val infinity = lty.infinity --
                    (if (!(isLt ^ pos)) InfinityTy.Pos else InfinityTy.Neg)
                  if (lty.math <= IntTy) rty.getSingle match
                    case One(Math(0)) =>
                      math = (isLt, pos) match
                        case (true, true)   => /* x < 0 */ NegIntTy
                        case (true, false)  => /* x >= 0 */ NonNegIntTy
                        case (false, true)  => /* x > 0 */ PosIntTy
                        case (false, false) => /* x <= 0 */ NonPosIntTy
                    case One(Math(v)) if v < 0 =>
                      math = (isLt, pos) match
                        case (true, true)   => /* x < N */ NegIntTy
                        case (true, false)  => /* x >= N */ IntTy
                        case (false, true)  => /* x > N */ IntTy
                        case (false, false) => /* x <= N */ NegIntTy
                    case One(Math(v)) if v > 0 =>
                      math = (isLt, pos) match
                        case (true, true)   => /* x < P */ IntTy
                        case (true, false)  => /* x >= P */ PosIntTy
                        case (false, true)  => /* x > P */ PosIntTy
                        case (false, false) => /* x <= P */ IntTy
                    case _ =>
                  val refinedTy = ValueTy(
                    math = math,
                    infinity = infinity,
                    number = lty.number,
                    bigInt = lty.bigInt,
                  )
                  if (lty != refinedTy) Some(refinedTy) else None
                }
                var lguard: TypeGuard = Map()
                toSymRef(l, lv).map { ref =>
                  aux(lty, rty, true, true).map { ty =>
                    lguard += True -> withPred(SymPred(Map(ref -> ty)))
                  }
                  aux(lty, rty, false, true).map { ty =>
                    lguard += False -> withPred(SymPred(Map(ref -> ty)))
                  }
                }
                var rguard: TypeGuard = Map()
                toSymRef(r, rv).map { ref =>
                  aux(rty, lty, true, false).map { ty =>
                    rguard += True -> withPred(SymPred(Map(ref -> ty)))
                  }
                  aux(rty, lty, false, false).map { ty =>
                    rguard += False -> withPred(SymPred(Map(ref -> ty)))
                  }
                }
                val guard = (for {
                  kind <- Set(True, False)
                  pred =
                    lguard.getOrElse(kind, SymPred()) &&
                      rguard.getOrElse(kind, SymPred())
                } yield kind -> withPred(pred)).toMap
                AbsValue(BoolT, Many, guard)
              })
            case EBinary(BOp.Eq, ERef(Field(x: Local, EStr(field))), expr) =>
              Some(for {
                rv <- transfer(expr)
                given AbsState <- get
                v <- refineField(x, field, Binding(rv.ty))
              } yield v)
            case EBinary(BOp.Eq, ERef(ref), expr) =>
              Some(for {
                lv <- transfer(ref)
                rv <- transfer(expr)
                given AbsState <- get
              } yield {
                val lty = lv.ty
                val rty = rv.ty
                val thenTy = lty && rty
                val elseTy = if (rty.isSingle) lty -- rty else lty
                var guard: TypeGuard = Map()
                toSymRef(ref, lv).map { ref =>
                  if (lty != thenTy)
                    guard += True -> withPred(SymPred(Map(ref -> thenTy)))
                  if (lty != elseTy)
                    guard += False -> withPred(SymPred(Map(ref -> elseTy)))
                }
                AbsValue(BoolT, Many, guard)
              })
            case ETypeCheck(ERef(ref), givenTy) =>
              Some(for {
                lv <- transfer(ref)
                given AbsState <- get
              } yield {
                val lty = lv.ty
                val rty = givenTy.toValue
                val thenTy = lty && rty
                val elseTy = lty -- rty
                var guard: TypeGuard = Map()
                var bools = Set(true, false)
                toSymRef(ref, lv).map { ref =>
                  if (lty != thenTy)
                    if (thenTy.isBottom) bools -= true
                    else guard += True -> withPred(SymPred(Map(ref -> thenTy)))
                  if (lty != elseTy)
                    if (elseTy.isBottom) bools -= false
                    else guard += False -> withPred(SymPred(Map(ref -> elseTy)))
                }
                AbsValue(BoolT(bools), Many, guard)
              })
            case EExists(Field(x: Local, EStr(field))) =>
              Some(refineField(x, field, Binding.Exist))
            case EBinary(BOp.Eq, ETypeOf(l), ETypeOf(r)) => None // TODO
            case EBinary(BOp.Eq, ETypeOf(ERef(ref)), expr) =>
              Some(for {
                lv <- transfer(ref)
                rv <- transfer(expr)
                given AbsState <- get
              } yield {
                val lty = lv.ty
                val rty = rv.ty
                def aux(positive: Boolean): ValueTy = rty.str.getSingle match
                  case One(tname) =>
                    val vty = ValueTy.fromTypeOf(tname)
                    if (positive) lty && vty else lty -- vty
                  case _ => lty
                val thenTy = aux(true)
                val elseTy = aux(false)
                var guard: TypeGuard = Map()
                toSymRef(ref, lv).map { ref =>
                  if (lty != thenTy)
                    guard += True -> withPred(SymPred(Map(ref -> thenTy)))
                  if (lty != elseTy)
                    guard += False -> withPred(SymPred(Map(ref -> elseTy)))
                }
                AbsValue(BoolT, Many, guard)
              })
            case EUnary(UOp.Not, e) =>
              Some(for {
                v <- transfer(e)
                given AbsState <- get
                ty = v.ty
                guard = v.guard
                lt = guard.get(True)
                lf = guard.get(False)
              } yield {
                var guard: TypeGuard = Map()
                lf.map { pred => guard += True -> withPred(pred) }
                lt.map { pred => guard += False -> withPred(pred) }
                AbsValue(ty.not, Many, guard)
              })
            case EBinary(BOp.Or, l, r) =>
              Some(for {
                lv <- transfer(l)
                st <- get
                given AbsState = st
                lty = lv.ty
                rv <- transfer(r)
                rty = rv.ty
                hasT = lty.bool.contains(true)
                lguard = lv.guard
                lt = lguard.getOrElse(True, SymPred())
                lf = lguard.getOrElse(False, SymPred())
              } yield {
                var guard: TypeGuard = Map()
                val refinedSt = if (lt.isTop) st else refine(lt, false)(st)
                val (thenPred, _) = (for {
                  rv <- transfer(r)
                  rt = rv.guard.getOrElse(True, SymPred())
                } yield if (hasT) lt || rt else rt)(refinedSt)
                if (thenPred.nonTop) guard += True -> withPred(thenPred)
                val (elsePred, _) = (for {
                  rv <- transfer(r)
                  rf = rv.guard.getOrElse(False, SymPred())
                } yield lf && rf)(refinedSt)
                if (elsePred.nonTop) guard += False -> withPred(elsePred)
                AbsValue(lty or rty, Many, guard)
              })
            case EBinary(BOp.And, l, r) =>
              Some(for {
                lv <- transfer(l)
                st <- get
                given AbsState = st
                lty = lv.ty
                rv <- transfer(r)
                rty = rv.ty
                hasF = lty.bool.contains(false)
                lguard = lv.guard
                lt = lguard.getOrElse(True, SymPred())
                lf = lguard.getOrElse(False, SymPred())
              } yield {
                var guard: TypeGuard = Map()
                val refinedSt = if (lt.isTop) st else refine(lt, true)(st)
                val (thenPred, _) = (for {
                  rv <- transfer(r)
                  rt = rv.guard.getOrElse(True, SymPred())
                } yield lt && rt)(refinedSt)
                if (thenPred.nonTop) guard += True -> withPred(thenPred)
                val (elsePred, _) = (for {
                  rv <- transfer(r)
                  rf = rv.guard.getOrElse(False, SymPred())
                } yield if (hasF) lf || rf else rf)(refinedSt)
                if (elsePred.nonTop) guard += False -> withPred(elsePred)
                AbsValue(lty and rty, Many, guard)
              })
            case _ => None
          }
      }
    }

    /** transfer function for references */
    def transfer(
      ref: Ref,
    )(using np: NodePoint[Node]): Result[AbsValue] = ref match
      case x: Var =>
        for {
          v <- get(_.get(x))
        } yield v
      case field @ Field(base, expr) =>
        for {
          b <- transfer(base)
          p <- transfer(expr)
          given AbsState <- get
          v <- get(_.get(b, p))
        } yield v

    /** transfer function for unary operators */
    def transfer(
      unary: EUnary,
      operand: AbsValue,
    )(using np: NodePoint[Node]): AbsValue = {
      import UOp.*
      given AbsState = getResult(np)
      if (config.checkUnaryOp)
        val operandTy = operand.ty
        unary.uop match
          case Abs | Floor =>
            checkUnary(unary, operandTy, MathT)
          case Neg | BNot =>
            checkUnary(unary, operandTy, MathT || NumberT || BigIntT)
          case Not =>
            checkUnary(unary, operandTy, BoolT)
      unary.uop match
        case Neg   => -operand
        case Not   => !operand
        case BNot  => ~operand
        case Abs   => operand.abs
        case Floor => operand.floor
    }

    private def checkUnary(
      unary: EUnary,
      operandTy: ValueTy,
      expectedTys: ValueTy,
    )(using np: NodePoint[Node]): Unit = if (!(operandTy <= expectedTys))
      addError(UnaryOpTypeMismatch(UnaryOpPoint(np, unary), operandTy))

    /** transfer function for binary operators */
    def transfer(
      binary: EBinary,
      left: AbsValue,
      right: AbsValue,
    )(using np: NodePoint[Node]): AbsValue = {
      import BOp.*
      given AbsState = getResult(np)
      if (config.checkBinaryOp)
        val (lhsTy, rhsTy) = (left.ty, right.ty)
        binary.bop match
          case Add | Sub | Mul | Pow | Div | UMod | Mod | Lt | Equal =>
            checkBinary(binary, lhsTy, rhsTy, Set(ExtMathT, NumberT, BigIntT))
          case LShift | SRShift | URShift | BAnd | BOr | BXOr =>
            checkBinary(binary, lhsTy, rhsTy, Set(MathT, BigIntT))
          case And | Or | Xor =>
            checkBinary(binary, lhsTy, rhsTy, Set(BoolT))
          case Eq =>
      binary.bop match {
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

    private def checkBinary(
      binary: EBinary,
      lhsTy: ValueTy,
      rhsTy: ValueTy,
      expectedTys: Set[ValueTy],
    )(using np: NodePoint[Node]): Unit =
      if (!expectedTys.exists(ty => lhsTy <= ty || rhsTy <= ty))
        val binaryPoint = BinaryOpPoint(np, binary)
        addError(BinaryOpTypeMismatch(binaryPoint, lhsTy, rhsTy))

    /** transfer for variadic operators */
    def transfer(
      vop: VOp,
      vs: List[AbsValue],
    )(using np: NodePoint[Node]): AbsValue =
      given AbsState = getResult(np)
      vop match
        case VOp.Min =>
          val math = vs.map(_.ty.math).reduce(_ min _)
          val inf = vs.map(_.ty.infinity).reduce(_ || _)
          AbsValue(
            ValueTy(
              math = math,
              infinity = if (math.isBottom) inf else inf && InfinityTy.Neg,
            ),
          )

        case VOp.Max =>
          val math = vs.map(_.ty.math).reduce(_ max _)
          val inf = vs.map(_.ty.infinity).reduce(_ || _)
          AbsValue(
            ValueTy(
              math = math,
              infinity = if (math.isBottom) inf else inf && InfinityTy.Pos,
            ),
          )
        case VOp.Concat => StrTop

    /** transfer for mathematical operators */
    def transfer(
      mop: MOp,
      vs: List[AbsValue],
    )(using np: NodePoint[Node]): AbsValue = MathTop

    // =========================================================================
    // Implementation for TyChecker
    // =========================================================================

    // -------------------------------------------------------------------------
    // Instantiation of Symbolic Expressions and References
    // -------------------------------------------------------------------------
    /** instantiation of symbolic predicate */
    def instantiate(pred: SymPred, map: Map[Sym, SymRef]): SymPred = SymPred(
      for {
        case (ref, ty) <- pred.map
        newRef <- instantiate(ref, map)
      } yield newRef -> ty,
      for {
        e <- pred.expr
        newExpr <- instantiate(e, map)
      } yield newExpr,
    )

    /** instantiation of symbolic expressions */
    def instantiate(sexpr: SymExpr, map: Map[Sym, SymRef]): Option[SymExpr] =
      import SymExpr.*
      sexpr match
        case SEBool(b) => Some(SEBool(b))
        case SEStr(s)  => Some(SEStr(s))
        case SERef(ref) =>
          for { r <- instantiate(ref, map) } yield SERef(r)
        case SETypeCheck(base, ty) =>
          for { b <- instantiate(base, map) } yield SETypeCheck(b, ty)
        case SEBinary(bop, left, right) =>
          for {
            l <- instantiate(left, map)
            r <- instantiate(right, map)
          } yield SEBinary(bop, l, r)
        case SEUnary(uop, expr) =>
          for { e <- instantiate(expr, map) } yield SEUnary(uop, e)

    /** instantiation of symbolic references */
    def instantiate(sref: SymRef, map: Map[Sym, SymRef]): Option[SymRef] =
      import SymRef.*
      sref match
        case SSym(sym) => map.get(sym)
        case SLocal(x) => None
        case SField(base, field) =>
          for {
            b <- instantiate(base, map)
            f <- instantiate(field, map)
          } yield SField(b, f)

    // -------------------------------------------------------------------------
    // Type Refinement
    // -------------------------------------------------------------------------
    /** refine condition */
    def refine(
      cond: Expr,
      positive: Boolean,
    )(using np: NodePoint[_]): Updater = cond match {
      // refine boolean local variables
      case ERef(x: Local) =>
        refineBool(x, positive)
      // refine inequality
      case EBinary(BOp.Lt, l, r) =>
        refineIneq(l, r, positive)
      // refine local variables
      case EBinary(BOp.Eq, ERef(x: Local), expr) =>
        refineLocal(x, expr, positive)
      // refine field equality
      case EBinary(BOp.Eq, ERef(Field(x: Local, EStr(field))), expr) =>
        refineField(x, field, expr, positive)
      // refine field existence
      case EExists(Field(x: Local, EStr(field))) =>
        refineExistField(x, field, positive)
      // refine types
      case EBinary(BOp.Eq, ETypeOf(ERef(x: Local)), expr) =>
        refineType(x, expr, positive)
      // refine type checks
      case ETypeCheck(ERef(ref), ty) =>
        refineTypeCheck(ref, ty.ty.toValue, positive)
      // refine logical negation
      case EUnary(UOp.Not, e) =>
        refine(e, !positive)
      // refine logical disjunction
      case EBinary(BOp.Or, l, r) =>
        st =>
          if (positive) refine(l, true)(st) ⊔ refine(r, true)(st)
          else refine(r, false)(refine(l, false)(st))
      // refine logical conjunction
      case EBinary(BOp.And, l, r) =>
        st =>
          if (positive) refine(r, true)(refine(l, true)(st))
          else refine(l, false)(st) ⊔ refine(r, false)(st)
      // no pruning
      case _ => st => st
    }

    /** refine types */
    def refine(
      value: AbsValue,
      refinedValue: AbsValue,
    )(using np: NodePoint[_]): Updater =
      import RefinementKind.*
      given AbsState = getResult(np)
      val refined = refinedValue.ty
      join(for {
        pred <- value.guard.collect {
          case (True, pred) if refined <= TrueT                  => pred
          case (False, pred) if refined <= FalseT                => pred
          case (Normal, pred) if refined <= NormalT              => pred
          case (Abrupt, pred) if refined <= AbruptT              => pred
          case (NormalTrue, pred) if refined <= NormalT(TrueT)   => pred
          case (NormalFalse, pred) if refined <= NormalT(FalseT) => pred
        }
      } yield refine(pred, true))

    /** refine types using symbolic predicates */
    def refine(
      pred: SymPred,
      positive: Boolean,
    )(using np: NodePoint[_]): Updater =
      import SymExpr.*, SymRef.*
      val SymPred(map, expr) = pred
      for {
        _ <- join(map.map { case (ref, ty) => refine(ref, ty, positive) })
        _ <- expr.fold(pure(()))(e => modify(refine(e, positive)))
        _ <- modify(st => st.copy(pred = st.pred && pred))
      } yield ()

    /** refine types using symbolic expressions */
    def refine(
      expr: SymExpr,
      positive: Boolean,
    )(using np: NodePoint[_]): Updater =
      import SymExpr.*, SymRef.*
      for {
        _ <- modify(expr match {
          // refine boolean local variables
          case SERef(x: Local) =>
            refineBool(x, positive)
          // refine type checks
          case SETypeCheck(SERef(ref), ty) =>
            refine(ref, ty, positive)
          // refine logical negation
          case SEUnary(UOp.Not, e) =>
            refine(e, !positive)
          // refine logical disjunction
          case SEBinary(BOp.Or, l, r) =>
            st =>
              if (positive) refine(l, true)(st) ⊔ refine(r, true)(st)
              // TODO short circuiting
              else refine(r, false)(refine(l, false)(st))
          // refine logical conjunction
          case SEBinary(BOp.And, l, r) =>
            st =>
              if (positive) refine(r, true)(refine(l, true)(st))
              // TODO short circuiting
              else refine(l, false)(st) ⊔ refine(r, false)(st)
          // no pruning
          case _ => st => st
        })
      } yield ()

    /** refine references using types */
    def refine(
      ref: SymRef,
      ty: ValueTy,
      positive: Boolean,
    )(using np: NodePoint[_]): Updater =
      import SymExpr.*, SymRef.*
      ref match
        case SSym(sym) =>
          st =>
            val refinedTy = st.symEnv.get(sym).fold(ty)(_ && ty)
            st.copy(symEnv = st.symEnv + (sym -> refinedTy))
        case SLocal(x) =>
          for {
            v <- transfer(x)
            given AbsState <- get
            refinedV =
              if (positive)
                if (v.ty <= ty.toValue) v
                else v ⊓ AbsValue(ty)
              else v -- AbsValue(ty)
            _ <- modify(_.update(x, refinedV, refine = true))
            _ <- refine(v, refinedV) // propagate type guard
          } yield ()
        case SField(base, SEStr(field)) =>
          for {
            bty <- get(_.getTy(base))
            rbinding = Binding(ty)
            binding = if (positive) rbinding else bty.record(field) -- rbinding
            refinedTy = ValueTy(
              ast = bty.ast,
              record = bty.record.update(field, binding, refine = true),
            )
            _ <- refine(base, refinedTy, positive)
          } yield ()
        case _ => st => st

    /** refine types for boolean local variables */
    def refineBool(
      x: Local,
      positive: Boolean,
    )(using np: NodePoint[_]): Updater = for {
      lv <- transfer(x)
      refinedV = if (positive) True else False
      _ <- modify(_.update(x, refinedV, refine = true))
      _ <- refine(lv, refinedV)
    } yield ()

    /** refine types with inequalities */
    def refineIneq(
      l: Expr,
      r: Expr,
      positive: Boolean,
    )(using np: NodePoint[_]): Updater =
      def toLocal(e: Expr): Option[Local] = e match
        case ERef(x: Local) => Some(x)
        case _              => None
      for {
        lv <- transfer(l)
        rv <- transfer(r)
        given AbsState <- get
        lmath = lv.ty.math
        rmath = rv.ty.math
        _ <- modify { st =>
          val lst = toLocal(l).fold(st) { x =>
            var math = lmath
            var infinity = lv.ty.infinity --
              (if (positive) InfinityTy.Pos else InfinityTy.Neg)
            val refined = (r, rmath) match
              case (EMath(0), _) =>
                math = if (positive) NegIntTy else NonNegIntTy
              case l =>
            st.update(
              x,
              AbsValue(
                ValueTy(
                  math = math,
                  infinity = infinity,
                  number = lv.ty.number,
                  bigInt = lv.ty.bigInt,
                ),
              ),
              refine = true,
            )
          }
          toLocal(r).fold(lst) { x =>
            var math = rmath
            var infinity = rv.ty.infinity --
              (if (positive) InfinityTy.Neg else InfinityTy.Pos)
            val refined = (l, lmath) match
              case (EMath(0), _) =>
                math = if (positive) PosIntTy else NonPosIntTy
              case _ => rmath
            lst.update(
              x,
              AbsValue(
                ValueTy(
                  math = math,
                  infinity = infinity,
                  number = rv.ty.number,
                  bigInt = rv.ty.bigInt,
                ),
              ),
              refine = true,
            )
          }
        }
      } yield ()

    /** refine types of local variables with equality */
    def refineLocal(
      x: Local,
      expr: Expr,
      positive: Boolean,
    )(using np: NodePoint[_]): Updater = for {
      rv <- transfer(expr)
      lv <- transfer(x)
      given AbsState <- get
      refinedV =
        if (positive) lv ⊓ rv
        else if (rv.isSingle) lv -- rv
        else lv
      _ <- modify(_.update(x, refinedV, refine = true))
      _ <- refine(lv, refinedV)
    } yield ()

    /** TODO refine types with field equality */
    def refineField(
      x: Local,
      field: String,
      expr: Expr,
      positive: Boolean,
    )(using np: NodePoint[_]): Updater = for {
      rv <- transfer(expr)
      given AbsState <- get
      _ <- refineField(x, field, Binding(rv.ty), positive)
    } yield ()

    def refineField(
      x: Local,
      field: String,
      rbinding: Binding,
      positive: Boolean,
    )(using np: NodePoint[_]): Updater = for {
      lv <- transfer(x)
      given AbsState <- get
      lty = lv.ty
      binding = if (positive) rbinding else lty.record(field) -- rbinding
      refinedTy = ValueTy(
        ast = lty.ast,
        record = lty.record.update(field, binding, refine = true),
      )
      refinedV = AbsValue(refinedTy)
      _ <- modify(_.update(x, refinedV, refine = true))
      _ <- refine(lv, refinedV)
    } yield ()

    /** refine types with field existence */
    def refineExistField(
      x: Local,
      field: String,
      positive: Boolean,
    )(using np: NodePoint[_]): Updater =
      refineField(x, field, Binding.Exist, positive)

    /** refine types with `typeof` constraints */
    def refineType(
      x: Local,
      expr: Expr,
      positive: Boolean,
    )(using np: NodePoint[_]): Updater = for {
      lv <- transfer(x)
      rv <- transfer(expr)
      given AbsState <- get
      lty = lv.ty
      rty = rv.ty
      refinedV = rty.str.getSingle match
        case One(tname) =>
          val value = AbsValue(ValueTy.fromTypeOf(tname))
          if (positive) lv ⊓ value else lv -- value
        case _ => lv
      _ <- modify(_.update(x, refinedV, refine = true))
    } yield ()

    /** refine types with type checks */
    def refineTypeCheck(
      ref: Ref,
      ty: ValueTy,
      positive: Boolean,
    )(using np: NodePoint[_]): Updater = for {
      v <- transfer(ref)
      given AbsState <- get
      refinedV =
        if (positive)
          if (v.ty <= ty.toValue) v
          else v ⊓ AbsValue(ty)
        else v -- AbsValue(ty)
      _ <- modify(ref match
        case x: Local => _.update(x, refinedV, refine = true)
        case Field(x: Local, EStr(field)) =>
          refineField(x, field, Binding(ty), positive)
        case _ => identity,
      )
      _ <- refine(v, refinedV)
    } yield ()

    /** check if the return type can be used */
    private lazy val canUseReturnTy: Func => Boolean = cached { func =>
      !func.retTy.isImprec ||
      typeGuards.contains(func.name) ||
      defaultTypeGuards.contains(func.name)
    }

    // -------------------------------------------------------------------------
    // Type Guards
    // -------------------------------------------------------------------------
    /** default type guards */
    val defaultTypeGuards: Set[String] = Set(
      "__APPEND_LIST__",
      "__FLAT_LIST__",
      "__GET_ITEMS__",
      "__CLAMP__",
      "Completion",
      "NormalCompletion",
      "UpdateEmpty",
    )

    /** type guards */
    type Refinements = Map[RefinementKind, Map[Local, ValueTy]]
    type Refinement = (List[AbsValue], ValueTy, AbsState) => AbsValue
    val typeGuards: Map[String, Refinement] = {
      import RefinementKind.*, SymExpr.*, SymRef.*
      Map(
        "__APPEND_LIST__" -> { (vs, retTy, st) =>
          given AbsState = st
          AbsValue(vs(0).ty || vs(1).ty, Zero, Map())
        },
        "__FLAT_LIST__" -> { (vs, retTy, st) =>
          given AbsState = st
          AbsValue(vs(0).ty.list.elem, Zero, Map())
        },
        "__GET_ITEMS__" -> { (vs, retTy, st) =>
          given AbsState = st
          val ast = vs(1).ty.toValue.grammarSymbol match
            case Fin(set) => AstT(set.map(_.name))
            case Inf      => AstT
          AbsValue(ListT(ast), Zero, Map())
        },
        "__CLAMP__" -> { (vs, retTy, st) =>
          given AbsState = st
          val refined =
            if (vs(0).ty.toValue <= (IntT || InfinityT))
              if (vs(1).ty.toValue <= MathT(0)) NonNegIntT
              else IntT
            else retTy
          AbsValue(refined, Zero, Map())
        },
        "Completion" -> { (vs, retTy, st) =>
          given AbsState = st
          vs(0) ⊓ AbsValue(CompT, Zero, Map())
        },
        "NormalCompletion" -> { (vs, retTy, st) =>
          given AbsState = st
          AbsValue(NormalT(vs(0).ty -- CompT), Zero, Map())
        },
        "IteratorClose" -> { (vs, retTy, st) =>
          given AbsState = st
          // Throw | #1
          AbsValue(vs(1).ty || ThrowT, Zero, Map())
        },
        "AsyncIteratorClose" -> { (vs, retTy, st) =>
          given AbsState = st
          // Throw | #1
          AbsValue(vs(1).ty || ThrowT, Zero, Map())
        },
        "OrdinaryObjectCreate" -> { (vs, retTy, st) =>
          given AbsState = st
          // Object
          AbsValue(RecordT("Object"), Zero, Map())
        },
        "UpdateEmpty" -> { (vs, retTy, st) =>
          given AbsState = st
          val record = vs(0).ty.record
          val valueField = record("Value").value
          val updated = record.update(
            "Value",
            vs(1).ty || (valueField -- EnumT("empty")),
            refine = false,
          )
          AbsValue(ValueTy(record = updated), Zero, Map())
        },
        "MakeBasicObject" -> { (vs, retTy, st) =>
          given AbsState = st
          AbsValue(RecordT("Object"), Zero, Map())
        },
        "Await" -> { (vs, retTy, st) =>
          given AbsState = st
          AbsValue(NormalT(ESValueT) || ThrowT, Zero, Map())
        },
        "IsCallable" -> { (vs, retTy, st) =>
          given AbsState = st
          val guard: TypeGuard = Map(
            True -> SymPred(Map(SSym(0) -> FunctionT)),
          )
          AbsValue(retTy, Zero, guard)
        },
        "IsConstructor" -> { (vs, retTy, st) =>
          given AbsState = st
          val guard: TypeGuard = Map(
            True -> SymPred(Map(SSym(0) -> ConstructorT)),
          )
          AbsValue(retTy, Zero, guard)
        },
        "RequireInternalSlot" -> { (vs, retTy, st) =>
          given AbsState = st
          val refined = vs(1).ty.str.getSingle match
            case One(f) =>
              ValueTy(
                record = ObjectT.record.update(f, Binding.Exist, refine = true),
              )
            case _ => ObjectT
          val guard: TypeGuard = Map(
            Normal -> SymPred(Map(SSym(0) -> refined)),
          )
          AbsValue(retTy, Zero, guard)
        },
        "ValidateTypedArray" -> { (vs, retTy, st) =>
          given AbsState = st
          val guard: TypeGuard = Map(
            Normal -> SymPred(Map(SSym(0) -> TypedArrayT)),
          )
          AbsValue(retTy, Zero, guard)
        },
        "ValidateIntegerTypedArray" -> { (vs, retTy, st) =>
          given AbsState = st
          val guard: TypeGuard = Map(
            Normal -> SymPred(Map(SSym(0) -> TypedArrayT)),
          )
          AbsValue(retTy, Zero, guard)
        },
        "ValidateAtomicAccessOnIntegerTypedArray" -> { (vs, retTy, st) =>
          given AbsState = st
          val guard: TypeGuard = Map(
            Normal -> SymPred(Map(SSym(0) -> TypedArrayT)),
          )
          AbsValue(retTy, Zero, guard)
        },
        "ValidateNonRevokedProxy" -> { (vs, retTy, st) =>
          given AbsState = st
          val guard: TypeGuard = Map(
            Normal -> SymPred(
              Map(
                SSym(0) -> ValueTy.from(
                  "Record[ProxyExoticObject { ProxyHandler : Record[Object], ProxyTarget : Record[Object] }]",
                ),
              ),
            ),
          )
          AbsValue(retTy, Zero, guard)
        },
        "IsPromise" -> { (vs, retTy, st) =>
          given AbsState = st
          val guard: TypeGuard = Map(
            True -> SymPred(Map(SSym(0) -> RecordT("Promise"))),
          )
          AbsValue(retTy, Zero, guard)
        },
        "IsRegExp" -> { (vs, retTy, st) =>
          given AbsState = st
          val guard: TypeGuard = Map(
            NormalTrue -> SymPred(Map(SSym(0) -> ObjectT)),
            Abrupt -> SymPred(Map(SSym(0) -> ObjectT)),
          )
          AbsValue(retTy, Zero, guard)
        },
        "NewPromiseCapability" -> { (vs, retTy, st) =>
          given AbsState = st
          val guard: TypeGuard = Map(
            Normal -> SymPred(Map(SSym(0) -> ConstructorT)),
          )
          AbsValue(retTy, Zero, guard)
        },
        "CreateListFromArrayLike" -> { (vs, retTy, st) =>
          given AbsState = st
          AbsValue(
            (for {
              v <- vs.lift(1)
              str = v.ty.list.elem.str
              ss <- str match
                case Inf     => None
                case Fin(ss) => Some(ss)
              ty = ss.map(ValueTy.fromTypeOf).foldLeft(BotT)(_ || _)
              refined = retTy.toValue && NormalT(ListT(ty))
            } yield refined).getOrElse(retTy),
            Zero,
            Map(),
          )
        },
        "IsUnresolvableReference" -> { (vs, retTy, st) =>
          given AbsState = st
          var guard: TypeGuard = Map(
            True -> SymPred(
              Map(
                SSym(0) ->
                RecordT(
                  "ReferenceRecord",
                  Map("Base" -> EnumT("unresolvable")),
                ),
              ),
            ),
            False -> SymPred(
              Map(
                SSym(0) ->
                RecordT(
                  "ReferenceRecord",
                  Map("Base" -> (ESValueT || RecordT("EnvironmentRecord"))),
                ),
              ),
            ),
          )
          AbsValue(retTy, Zero, guard)
        },
        "IsPropertyReference" -> { (vs, retTy, st) =>
          given AbsState = st
          var guard: TypeGuard = Map(
            True ->
            SymPred(
              Map(
                SSym(0) -> RecordT("ReferenceRecord", Map("Base" -> ESValueT)),
              ),
            ),
            False -> SymPred(
              Map(
                SSym(0) ->
                RecordT(
                  "ReferenceRecord",
                  Map(
                    "Base" ->
                    (RecordT("EnvironmentRecord") || EnumT("unresolvable")),
                  ),
                ),
              ),
            ),
          )
          AbsValue(retTy, Zero, guard)
        },
        "IsSuperReference" -> { (vs, retTy, st) =>
          given AbsState = st
          val guard: TypeGuard = Map(
            True -> SymPred(Map(SSym(0) -> RecordT("SuperReferenceRecord"))),
          )
          AbsValue(retTy, Zero, guard)
        },
        "IsPrivateReference" -> { (vs, retTy, st) =>
          given AbsState = st
          val guard: TypeGuard = Map(
            True -> SymPred(
              Map(
                SSym(0) ->
                RecordT(
                  "ReferenceRecord",
                  Map("ReferencedName" -> RecordT("PrivateName")),
                ),
              ),
            ),
            False -> SymPred(
              Map(
                SSym(0) ->
                RecordT(
                  "ReferenceRecord",
                  Map(
                    "ReferencedName" -> (SymbolT || StrT /* TODO ESValue in latest version */ ),
                  ),
                ),
              ),
            ),
          )
          AbsValue(retTy, Zero, guard)
        },
        "IsArray" -> { (vs, retTy, st) =>
          given AbsState = st
          val guard: TypeGuard = Map(
            NormalTrue -> SymPred(Map(SSym(0) -> ObjectT)),
            Abrupt -> SymPred(Map(SSym(0) -> ObjectT)),
          )
          AbsValue(retTy, Zero, guard)
        },
        "IsSharedArrayBuffer" -> { (vs, retTy, st) =>
          given AbsState = st
          val guard: TypeGuard = Map(
            True -> SymPred(
              Map(
                SSym(0) ->
                RecordT(
                  "SharedArrayBuffer",
                  Map("ArrayBufferData" -> RecordT("SharedDataBlock")),
                ),
              ),
            ),
          )
          AbsValue(retTy, Zero, guard)
        },
        "IsConcatSpreadable" -> { (vs, retTy, st) =>
          given AbsState = st
          val guard: TypeGuard = Map(
            NormalTrue -> SymPred(Map(SSym(0) -> ObjectT)),
            Abrupt -> SymPred(Map(SSym(0) -> ObjectT)),
          )
          AbsValue(retTy, Zero, guard)
        },
        "IsDetachedBuffer" -> { (vs, retTy, st) =>
          given AbsState = st
          def getTy(ty: ValueTy, sty: ValueTy) = RecordT(
            Map(
              "ArrayBuffer" -> FieldMap("ArrayBufferData" -> Binding(ty)),
              "SharedArrayBuffer" -> FieldMap("ArrayBufferData" -> Binding(sty)),
            ),
          )
          val guard: TypeGuard = Map(
            True -> SymPred(Map(SSym(0) -> getTy(NullT, NullT))),
            False -> SymPred(
              Map(
                SSym(0) -> getTy(
                  RecordT("DataBlock"),
                  RecordT("SharedDataBlock"),
                ),
              ),
            ),
          )
          AbsValue(retTy, Zero, guard)
        },
        "AllocateArrayBuffer" -> { (vs, retTy, st) =>
          given AbsState = st
          AbsValue(
            NormalT(
              RecordT(
                "ArrayBuffer",
                FieldMap("ArrayBufferData" -> Binding(RecordT("DataBlock"))),
              ),
            ) || ThrowT,
            Zero,
            Map(),
          )
        },
        "AllocateSharedArrayBuffer" -> { (vs, retTy, st) =>
          given AbsState = st
          AbsValue(
            NormalT(
              RecordT(
                "SharedArrayBuffer",
                FieldMap(
                  "ArrayBufferData" -> Binding(RecordT("SharedDataBlock")),
                ),
              ),
            ) || ThrowT,
            Zero,
            Map(),
          )
        },
        "CanBeHeldWeakly" -> { (vs, retTy, st) =>
          given AbsState = st
          val guard: TypeGuard = Map(
            True -> SymPred(Map(SSym(0) -> (ObjectT || SymbolT))),
          )
          AbsValue(retTy, Zero, guard)
        },
        "AsyncGeneratorValidate" -> { (vs, retTy, st) =>
          given AbsState = st
          val guard: TypeGuard = Map(
            Normal -> SymPred(Map(SSym(0) -> RecordT("AsyncGenerator"))),
          )
          AbsValue(retTy, Zero, guard)
        },
      )
    }
  }
}
