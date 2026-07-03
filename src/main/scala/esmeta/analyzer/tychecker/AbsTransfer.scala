package esmeta.analyzer.tychecker

import esmeta.cfg.{util => _, *}
import esmeta.ir.{Func => _, util => _, *}
import esmeta.state.*
import esmeta.ty.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import scala.annotation.tailrec
import esmeta.es.builtin.JOB_QUEUE

trait AbsTransferDecl { analyzer: TyChecker =>

  /** abstract transfer function */
  class AbsTransfer extends AbsTransferLike {

    /** loading monads */
    import monad.*

    /** loading predefined abstract values */
    import AbsValue.*

    /** loading constructors */
    import SymTy.*, Property.*

    // =========================================================================
    // abstract trasnfer function
    // =========================================================================
    /** transfer function for node points */
    def apply(np: NodePoint[?]): Unit =
      // record current control point for alarm
      val st = getResult(np)
      given NodePoint[?] = np
      val NodePoint(func, node, view) = np
      node match
        case Block(_, insts, next) =>
          val newSt = insts.foldLeft(st) {
            case (nextSt, _) if nextSt.isBottom => AbsState.Bot
            case (nextSt, inst)                 => transfer(inst)(nextSt)
          }
          for (to <- next) analyzer += getNextNp(np, to) -> newSt
        case call @ Call(_, _, next) =>
          val (v, newSt) = transfer(call)(st)
          for (to <- next if !v.isBottom)
            analyzer += getNextNp(np, to) -> newSt.define(call.lhs, v)
        case br @ Branch(_, kind, cond, _, thenNode, elseNode, _) =>
          val (v, newSt) = transfer(cond)(st)
          for (node <- thenNode if v.ty(using st).contains(true))
            analyzer += getNextNp(np, node) -> refine(v, TrueT)(newSt)
          for (node <- elseNode if v.ty(using st).contains(false))
            analyzer += getNextNp(np, node) -> refine(v, FalseT)(newSt)

    /** get next node point */
    def getNextNp(fromCp: NodePoint[Node], to: Node): NodePoint[Node] =
      fromCp.copy(node = to)

    /** transfer function for normal instructions */
    def transfer(
      inst: NormalInst,
    )(using np: NodePoint[?]): Updater = inst match {
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
          given AbsState <- get
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
      case IAssign(ref, expr)  => st => st /* TODO */
      case IExpand(base, expr) => st => st /* TODO */
      case IDelete(base, expr) => st => st /* TODO */
      case IPush(expr, ERef(list: Local), _) =>
        for {
          given AbsState <- get
          l <- transfer(list)
          v <- transfer(expr)
          elem = l.ty.list.elem || v.ty
          newV = AbsValue(ListT(elem))
          _ <- modify(_.update(list, newV, refine = false))
        } yield ()
      case IPush(expr, list, _) => st => st /* TODO */
      case IPop(lhs, list, front) =>
        for {
          v <- transfer(list)
          pv <- id(_.pop(v, front))
          _ <- modify(_.define(lhs, pv))
        } yield ()
      case inst @ IReturn(expr) =>
        for {
          v <- transfer(expr)
          st <- get
          _ <- doReturn(inst, st, v)
          _ <- put(AbsState.Bot)
        } yield ()
      case IAssert(expr: EYet) =>
        st => st /* skip not yet compiled assertions */
      case IAssert(expr) =>
        for {
          v <- transfer(expr)
          st <- get
          block = np.node match
            case block: Block => Some(block)
            case _            => None
          _ <- modify(refine(v, TrueT))
          refinedSt <- get
          given AbsState = refinedSt
          _ <- if (v ⊑ False) put(AbsState.Bot) else pure(())
        } yield ()
      case IPrint(expr) => st => st /* skip */
      case INop(_)      => st => st /* skip */
    }

    /** update return points */
    def doReturn(
      irReturn: Return,
      givenSt: AbsState,
      v: AbsValue,
    )(using np: NodePoint[Node]): Unit = {
      given AbsState = givenSt
      val NodePoint(func, node, view) = np
      val irp = InternalReturnPoint(func, node, irReturn)
      val entryView = getEntryView(view)
      val entryNp = NodePoint(func, func.entry, entryView)
      val entrySt = getResult(entryNp)
      val givenV = v.forReturn(givenSt, func, entrySt)
      val rp = ReturnPoint(func, entryView)
      val newV = func.retTy.ty match
        case _: UnknownTy        => givenV
        case expectedTy: ValueTy =>
          // return type check when it is a known type
          val givenTy = givenV.ty(using entrySt)
          if (givenTy <= expectedTy) givenV
          else
            if (config.checkReturnType)
              addError(ReturnTypeMismatch(irp, givenTy))
            AbsValue(STy(givenTy && expectedTy), givenV.guard)
      // no propagation if the return value is bottom
      if (!newV.isBottom)
        val AbsRet(oldV, noSym @ (noSymV, noSymConstr), syms) = getResult(rp)
        if (!oldV.isBottom && useRepl) Repl.merged = true
        if ((newV !⊑ oldV)(using entrySt)) {
          val constr = givenSt.constr.onlySym
          val hasSym = v.symty.hasSym
          val newRet = AbsRet(
            oldV ⊔ newV,
            if (hasSym) noSym else (noSymV ⊔ newV, noSymConstr || constr),
            if (hasSym) syms + (np -> (v.onlySym(using givenSt), constr))
            else syms - np,
          )
          rpMap += rp -> newRet
          worklist += rp
        }
    }

    /** transfer function for expressions */
    def transfer(
      expr: Expr,
    )(using np: NodePoint[Node]): Result[AbsValue] = st => {
      val (v, newSt) = (for {
        v <- basicTransfer(expr)
        given AbsState <- get
        guard <- getTypeGuard(expr)
        newV = v.addGuard(guard)
      } yield newV)(st)
      // No propagation if the result of the expression is bottom
      if (v.isBottom) (v, AbsState.Bot) else (v, newSt)
    }

    /** transfer function for expressions */
    def basicTransfer(
      expr: Expr,
    )(using np: NodePoint[Node]): Result[AbsValue] = expr match {
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
        else AbsValue.Bot
      case EContains(list, elem) =>
        for {
          l <- transfer(list)
          v <- transfer(elem)
          given AbsState <- get
        } yield
          if (l.ty.list.isBottom) AbsValue.Bot
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
      case ERef(ref) =>
        for {
          v <- transfer(ref)
        } yield v
      case unary @ EUnary(_, expr) =>
        for {
          v <- transfer(expr)
          st <- get
          v0 <- transfer(st, unary, v)
        } yield v0
      case binary @ EBinary(BOp.And | BOp.Or, left, right) =>
        shortCircuit(binary, left, right)
      case binary @ EBinary(_, left, right) =>
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
        import COp.*
        for {
          v <- transfer(expr)
          r <- cop match
            case ToStr(Some(radix)) => transfer(radix)
            case ToStr(None)        => pure(AbsValue(MathT(10)))
            case _                  => pure(AbsValue.Bot)
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
          b <- get(_.typeCheck(v, ty.toValue))
        } yield AbsValue(b)
      case ESizeOf(expr) =>
        for {
          v <- transfer(expr)
          given AbsState <- get
        } yield v.sizeOf
      case EClo(fname, captured) => AbsValue(CloT(fname))
      case ECont(fname)          => AbsValue(ContT(cfg.fnameMap(fname).id))
      case EDebug(expr) =>
        for {
          v <- transfer(expr)
          st <- get
          _ = debug(s"[[ $expr @ $np ]]($st) = $v")
        } yield v
      case ERandom() => pure(NumberTop)
      case ESyntactic(name, _, rhsIdx, _) =>
        pure(AbsValue(AstT(name, rhsIdx)))
      case ELexical(name, expr) => pure(AbsValue(AstT))
      case ERecord(
            tname @ "CompletionRecord",
            List(
              ("Type", EEnum("normal")),
              ("Value", expr),
              ("Target", EEnum("empty")),
            ),
          ) =>
        for {
          v <- transfer(expr)
          given AbsState <- get
          newV = v.symty match
            case STy(sty) => AbsValue(NormalT(sty))
            case s        => AbsValue(SNormal(s))
        } yield newV
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
      case ENumber(n) if n.isNaN => AbsValue(NumberT(Double.NaN))
      case ENumber(n)            => AbsValue(NumberT(n))
      case EBigInt(n)            => AbsValue(BigIntT)
      case EStr(str)             => AbsValue(StrT(str))
      case EBool(b)              => AbsValue(BoolT(b))
      case EUndef()              => AbsValue(UndefT)
      case ENull()               => AbsValue(NullT)
      case EEnum(name)           => AbsValue(EnumT(name))
      case ECodeUnit(c)          => AbsValue(CodeUnitT)
    }

    // short circuit evaluation
    def shortCircuit(
      binary: EBinary,
      left: Expr,
      right: Expr,
    )(using np: NodePoint[Node]): Result[AbsValue] = for {
      l <- transfer(left)
      given AbsState <- get
      v <- binary.bop match {
        case BOp.And =>
          val r: Result[AbsValue] = (st: AbsState) =>
            var bools = Set[Boolean]()
            val lbools = l.ty.bool.set
            if (lbools.contains(false)) bools += false
            if (lbools.contains(true)) {
              val block = np.node match
                case block: Block => Some(block)
                case _            => None
              val (r, _) = transfer(right)(refine(l, TrueT)(st))
              bools ++= r.ty.bool.set
            }
            (AbsValue(BoolT(bools)), st)
          r
        case BOp.Or =>
          val r: Result[AbsValue] = (st: AbsState) =>
            var bools = Set[Boolean]()
            val lbools = l.ty.bool.set
            if (lbools.contains(true)) bools += true
            if (lbools.contains(false))
              val (r, _) = transfer(right)(refine(l, FalseT)(st))
              bools ++= r.ty.bool.set
            (AbsValue(BoolT(bools)), st)
          r
        case _ =>
          for {
            r <- transfer(right)
            st <- get
            v = transfer(st, binary, l, r)
          } yield v
      }
    } yield v

    /** transfer function for call instructions */
    def transfer(call: Call)(using np: NodePoint[?]): Result[AbsValue] = {
      val callerNp = np.copy(node = call)
      call.callInst match {
        case ICall(_, fexpr, args) =>
          for {
            fv <- transfer(fexpr)
            st <- get
            given AbsState = st
            fty = fv.ty
            vs <- join(args.map(transfer))
          } yield {
            val cloRes = fty.clo match
              case CloTopTy           => AnyT
              case CloArrowTy(_, ret) => ret
              case CloSetTy(names) =>
                for {
                  fname <- names
                  f <- cfg.fnameMap.get(fname)
                } {
                  val captured: Map[Name, AbsValue] = Map() // TODO
                  doCall(callerNp, f, st, args, vs, captured, f.isMethod)
                }
                BotT
            val contRes = fty.cont match
              case Inf => AnyT
              case Fin(fids) =>
                for {
                  fid <- fty.cont.toIterable(stop = false)
                  f <- cfg.funcMap.get(fid)
                  tgt = Some(NodePoint(f, f.entry, emptyView))
                } {
                  val captured: Map[Name, AbsValue] = Map() // TODO
                  doCall(callerNp, f, st, args, vs, captured, f.isMethod, tgt)
                }
                BotT
            AbsValue(cloRes || contRes)
          }
        case ISdoCall(_, base, method, args) =>
          for {
            bv <- transfer(base)
            vs <- join(args.map(transfer))
            st <- get
            given AbsState = st
            bty = bv.ty
          } yield {
            var newV: AbsValue = AbsValue.Bot
            // lexical sdo
            newV ⊔= bv.getLexical(method)
            // syntactic sdo
            for ((sdo, ast) <- bv.getSdo(method))
              doCall(callerNp, sdo, st, base :: args, ast :: vs, method = true)

            newV
          }
      }
    }

    /** handle calls */
    def doCall(
      callerNp: NodePoint[Call],
      callee: Func,
      callerSt: AbsState,
      args: List[Expr],
      vs: List[AbsValue],
      captured: Map[Name, AbsValue] = Map(),
      method: Boolean = false,
      contTarget: Option[NodePoint[Node]] = None,
    ): Unit = {
      given AbsState = callerSt
      callInfo += callerNp -> callerSt
      analyzer.argsInfo += callerNp -> vs
      if (canUseReturnTy(callee)) {
        val call = callerNp.node
        val retTy = callee.retTy.ty.toValue
        var newRetV = (for {
          refiner <- manualRefiners.get(callee.name)
          v = refiner(callee, vs, retTy, callerSt)
          newV = instantiate(v, callerNp)
        } yield newV).getOrElse(AbsValue(retTy).lift)
        for {
          nextNp <- getAfterCallNp(callerNp)
          newSt = callerSt.define(call.lhs, newRetV)
        } analyzer += nextNp -> newSt
      }
      // get locals
      val locals = getLocals(callerNp, callee, method, vs) ++ captured
      // keep caller state to restore it
      contTarget match
        case Some(target) =>
          analyzer += target -> getCalleeState(callerSt, locals, callee)
        case None =>
          for {
            (calleeView, newLocals) <- getCalleeEntries(callerNp, locals)
            calleeSt = getCalleeState(callerSt, newLocals, callee)
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

    /** get local variables */
    def getLocals(
      callerNp: NodePoint[Call],
      callee: Func,
      method: Boolean,
      vs: List[AbsValue],
    ): List[(Local, AbsValue)] = {
      val arity @ (from, to) = callee.arity
      val len = vs.length
      if (config.checkArity && (len < from || to < len))
        val callPoint = CallPoint(callerNp.func, callerNp.node, callee)
        addError(ArityMismatch(callPoint, len))
      // get parameters
      val params: List[Param] = callee.irFunc.params
      // full arguments with optional parameters
      // construct local type environment
      (for {
        ((param, arg), idx) <- (params zip vs).zipWithIndex
      } yield param.lhs -> assignArg(callerNp, callee, method, idx, param, arg))
    }

    /** assign argument to parameter */
    def assignArg(
      callerNp: NodePoint[Call],
      callee: Func,
      method: Boolean,
      idx: Int,
      param: Param,
      arg: AbsValue,
    ): AbsValue =
      given AbsState = getResult(callerNp)
      val paramTy = param.ty.ty.toValue
      val argTy = arg.ty
      if (method && idx == 0) () /* ignore `this` for method calls */
      else if (config.checkParamType && !(argTy <= paramTy))
        val callPoint = CallPoint(callerNp.func, callerNp.node, callee)
        addError(ParamTypeMismatch(ArgAssignPoint(callPoint, idx), argTy))
      AbsValue(paramTy && argTy)

    /** callee entries */
    def getCalleeEntries(
      callerNp: NodePoint[Call],
      locals: List[(Local, AbsValue)],
    ): List[(View, List[(Local, AbsValue)])] = {
      given AbsState = getResult(callerNp)
      List(emptyView -> locals.map { (x, v) => x -> AbsValue(v.ty) })
    }

    /** propagate callee analysis result */
    def propagate(rp: ReturnPoint, callerNp: NodePoint[Call]): Unit = {
      if (!canUseReturnTy(rp.func)) {
        val ReturnPoint(func, view) = rp
        val entryView = getEntryView(view)
        val entryNp = NodePoint(func, func.entry, entryView)
        val entrySt = getResult(entryNp)
        given AbsState = entrySt
        val value = getResult(rp).value
        (for {
          nextNp <- getAfterCallNp(callerNp)
          callerSt = callInfo(callerNp)
          given AbsState = callerSt
          retTy = rp.func.retTy.ty.toValue
          newV = instantiate(value, callerNp) ⊓ AbsValue(retTy)
          if !newV.isBottom
        } yield analyzer += nextNp -> callerSt
          .define(callerNp.node.lhs, newV))
          .getOrElse {
            if (!getResult(rp).isBottom) worklist += rp
          }
      }
    }

    /** transfer function for return points */
    def apply(rp: ReturnPoint): Unit = if (!canUseReturnTy(rp.func)) {
      val ReturnPoint(func, view) = rp
      val entryNp = NodePoint(func, func.entry, emptyView)
      val entrySt = getResult(entryNp)
      val value = getResult(rp).value
      for {
        callerNps <- retEdges.get(rp)
        callerNp <- callerNps
        nextNp <- getAfterCallNp(callerNp)
      } {
        val callerSt: AbsState = callInfo(callerNp)
        val retV = AbsValue(rp.func.retTy.ty.toValue)
        val newV = (instantiate(value, callerNp) ⊓ retV)(using callerSt)
        val nextSt = callerSt.update(callerNp.node.lhs, newV, refine = false)
        analyzer += nextNp -> nextSt
      }
    }

    /** get after call node point */
    def getAfterCallNp(callerNp: NodePoint[Call]): Option[NodePoint[Node]] =
      callerNp.node.next.map(nextNode => callerNp.copy(node = nextNode))

    // =========================================================================
    // type refinements based on given facts
    // =========================================================================
    def refine(v: AbsValue, ty: ValueTy)(using
      np: NodePoint[?],
    ): Updater = st =>
      import TargetType.*
      val dty = TargetType(ty)
      val vty = v.ty(using st)
      val constr = v.guard.derive(vty, dty.ty)
      if (vty distinct ty) AbsState.Bot
      else refine(constr)(st)

    /** refine types using type constraints */
    def refine(constr: TypeConstr)(using np: NodePoint[?]): Updater =
      constr.fold[Updater](_ => AbsState.Bot) { map =>
        for {
          _ <- join(map.map { (x, ty) => modify(refine(x, ty)) })
          _ <- modify(st => st.copy(constr = st.constr && constr))
        } yield ()
      }

    /** refine references using types */
    def refine(
      ref: Base,
      ty: ValueTy,
    )(using np: NodePoint[?]): Updater = ref match
      case sym: Sym =>
        st =>
          val refinedTy = st.symEnv.get(sym).fold(ty)(_ && ty)
          st.copy(symEnv = st.symEnv + (sym -> refinedTy))
      case x: Local =>
        for {
          v <- get(_.get(x))
          given AbsState <- get
          refinedV = if (v.ty <= ty.toValue) v else v ⊓ AbsValue(ty)
          _ <- modify(_.update(x, refinedV, refine = true))
          _ <- refine(v, refinedV.ty) // propagate type guard
        } yield ()

    // =========================================================================
    // instantiation of symbolic values
    // =========================================================================
    /** instantiation of return value */
    def instantiate(
      value: AbsValue,
      callerNp: NodePoint[Call],
    ): AbsValue =
      given AbsState = callInfo(callerNp)
      val vs = analyzer.argsInfo.getOrElse(callerNp, Nil)
      val argsMap = vs.zipWithIndex.map { (v, i) => i -> v }.toMap
      instantiate(value, argsMap).lift

    /** instantiation of abstract values */
    def instantiate(
      value: AbsValue,
      argsMap: Map[Sym, AbsValue],
    )(using st: AbsState): AbsValue =
      val AbsValue(symty, guard) = value
      val newV = instantiate(symty, argsMap)
      val newGuard = instantiate(guard, argsMap).normalized(newV.ty)
      newV.addGuard(newGuard)

    def instantiate(
      guard: TypeGuard,
      argsMap: Map[Sym, AbsValue],
    )(using st: AbsState): TypeGuard = TypeGuard(
      guard.map.map((dty, constr) => dty -> instantiate(constr, argsMap)),
    )

    def instantiate(
      constr: TypeConstr,
      argsMap: Map[Sym, AbsValue],
    )(using st: AbsState): TypeConstr = instantiateConstr(constr, argsMap).lift

    /** instantiation of type constraints */
    def instantiateConstr(
      constr: TypeConstr,
      argsMap: Map[Sym, AbsValue],
    )(using st: AbsState): TypeConstr =
      def aux(x: Sym, ty: ValueTy): Option[(Base, ValueTy)] = for {
        v <- argsMap.get(x)
        y <- v.symty match
          case x: SymRef => Some(x)
          case _         => None
        (z, zty) <- toBase(y, ty)
      } yield z -> zty
      constr.map { map =>
        for {
          case (x: Sym, ty: ValueTy) <- map
          pair <- aux(x, ty)
        } yield pair
      }

    /** instantiation of symbolic type */
    def instantiate(
      symty: SymTy,
      argsMap: Map[Sym, AbsValue],
    )(using st: AbsState): AbsValue = symty match
      case STy(ty) => AbsValue(ty)
      case SVar(x) => AbsValue.Bot
      case SSym(s) => argsMap.getOrElse(s, AbsValue.Bot)
      case SField(b, f) =>
        st.get(instantiate(b, argsMap), instantiate(f, argsMap))
      case SProp(b, p) =>
        st.getProp(instantiate(b, argsMap), p)
      case SCall(b) =>
        st.getCall(instantiate(b, argsMap))
      case SConstruct(b) =>
        st.getConstruct(instantiate(b, argsMap))
      case SNormal(symty) =>
        val ty = instantiate(symty, argsMap).symty match
          case STy(ty) => STy(NormalT(ty))
          case s       => SNormal(s)
        AbsValue(ty)

    // =========================================================================
    // SymRef <: ValueTy --> (Base <: ValueTy)
    // =========================================================================
    // e.g. #0.f.g ({ f: { g: Number | String } }) <: Number
    // -->  #0 <: { f: { g: Number } }
    def toBase(
      ref: SymRef,
      givenTy: ValueTy,
    )(using st: AbsState): Option[(Base, ValueTy)] = ref match
      case SVar(x) => Some(x -> givenTy)
      case SSym(s) => Some(s -> givenTy)
      case SField(base, STy(x)) =>
        val field = x.str.getSingle match
          case One(elem) => elem
          case _         => return None
        val bty = base.ty
        val refined = ValueTy(
          ast = bty.ast,
          record = bty.record.update(field, givenTy, refine = true),
        )
        toBase(base, refined)
      case SProp(base, prop) =>
        val desc = Desc(
          getExc = givenTy overlap ThrowT,
          ty =
            if (givenTy overlap NormalT) st.get(givenTy, StrT("Value"))
            else BotT,
        )
        toBase(base, ValueTy(record = ObjectT.record.update(prop, desc)))
      case SCall(base) =>
        val call = CallDesc.Elem(
          exc = givenTy overlap ThrowT,
          ret =
            if (givenTy overlap NormalT) st.get(givenTy, StrT("Value"))
            else BotT,
        )
        toBase(base, ValueTy(record = ObjectT.record.update(call)))
      case SConstruct(base) =>
        val construct = ConstructDesc.Elem(
          exc = givenTy overlap ThrowT,
          ret =
            if (givenTy overlap NormalT) st.get(givenTy, StrT("Value"))
            else BotT,
        )
        toBase(base, ValueTy(record = ObjectT.record.update(construct)))
      case _ => None

    // =========================================================================
    // conversion an expression with its abstract value to symbolic references
    // =========================================================================
    /** conversion to symbolic references */
    def toSymRef(expr: Expr, value: AbsValue): Option[SymRef] =
      value.symty match
        case ref: SymRef => Some(ref)
        case _ =>
          expr match
            case ERef(ref) => toSymRef(ref)
            case _         => None

    /** conversion to symbolic references */
    def toSymRef(ref: Ref, value: AbsValue): Option[SymRef] =
      value.symty match
        case ref: SymRef => Some(ref)
        case _           => toSymRef(ref)

    /** conversion to symbolic references */
    def toSymRef(ref: Ref): Option[SymRef] = ref match
      case x: Local => Some(SVar(x))
      case Field(base, EStr(field)) =>
        for {
          b <- toSymRef(base)
        } yield SField(b, STy(StrT(field)))
      case _ => None

    // =========================================================================
    // type guard for expressions
    // =========================================================================
    def getTypeGuard(expr: Expr)(using np: NodePoint[?]): Result[TypeGuard] = {
      import TargetType.*
      given Node = np.node
      def toConstr(p: (Base, ValueTy))(using AbsState): TypeConstr =
        TypeConstr(p).lift
      expr match {
        case EBool(bool) =>
          val dty = if (bool) TargetType(TrueT) else TargetType(FalseT)
          get(st => TypeGuard(Map(dty -> TypeConstr.Top.lift(using st))))
        case ERecord(tname @ "CompletionRecord", fields) =>
          for {
            pairs <- join(fields.map {
              case (f, expr) =>
                for {
                  v <- transfer(expr)
                } yield (f, v)
            })
            v <- id(_.allocRecord(tname, pairs))
            given AbsState <- get
          } yield v.lift.guard
        case EBinary(BOp.Lt, l, r) =>
          for {
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
              var number = lty.number
              if (lty.math <= MathTy.Int) rty.getSingle match
                case One(Math(0)) =>
                  math = (isLt, pos) match
                    case (true, true)   => /* x < 0 */ MathTy.NegInt
                    case (true, false)  => /* x >= 0 */ MathTy.NonNegInt
                    case (false, true)  => /* x > 0 */ MathTy.PosInt
                    case (false, false) => /* x <= 0 */ MathTy.NonPosInt
                case One(Math(v)) if v < 0 =>
                  math = (isLt, pos) match
                    case (true, true)   => /* x < N */ MathTy.NegInt
                    case (true, false)  => /* x >= N */ MathTy.Int
                    case (false, true)  => /* x > N */ MathTy.Int
                    case (false, false) => /* x <= N */ MathTy.NegInt
                case One(Math(v)) if v > 0 =>
                  math = (isLt, pos) match
                    case (true, true)   => /* x < P */ MathTy.Int
                    case (true, false)  => /* x >= P */ MathTy.PosInt
                    case (false, true)  => /* x > P */ MathTy.PosInt
                    case (false, false) => /* x <= P */ MathTy.Int
                case _ =>
              if (lty.number <= NumberTy.Int) rty.getSingle match
                case One(Number(0)) =>
                  number = (isLt, pos) match
                    case (true, true)   => /* x < 0 */ NumberTy.NegInt
                    case (true, false)  => /* x >= 0 */ NumberTy.NonNegInt
                    case (false, true)  => /* x > 0 */ NumberTy.PosInt
                    case (false, false) => /* x <= 0 */ NumberTy.NonPosInt
                case One(Number(v)) if v < 0 =>
                  number = (isLt, pos) match
                    case (true, true)   => /* x < N */ NumberTy.NegInt
                    case (true, false)  => /* x >= N */ NumberTy.Int
                    case (false, true)  => /* x > N */ NumberTy.Int
                    case (false, false) => /* x <= N */ NumberTy.NegInt
                case One(Number(v)) if v > 0 =>
                  number = (isLt, pos) match
                    case (true, true)   => /* x < P */ NumberTy.Int
                    case (true, false)  => /* x >= P */ NumberTy.PosInt
                    case (false, true)  => /* x > P */ NumberTy.PosInt
                    case (false, false) => /* x <= P */ NumberTy.Int
                case _ =>
              val refinedTy = ValueTy(
                math = math,
                infinity = infinity,
                number = number,
                bigInt = lty.bigInt,
              )
              if (lty != refinedTy) Some(refinedTy) else None
            }
            var lmap: Map[TargetType, TypeConstr] = Map()
            toSymRef(l, lv).map { ref =>
              aux(lty, rty, true, true).map { thenTy =>
                if (lty != thenTy && !thenTy.isBottom)
                  toBase(ref, thenTy).map { pair =>
                    lmap += TargetType(TrueT) -> toConstr(pair)
                  }
              }
              aux(lty, rty, false, true).map { elseTy =>
                if (lty != elseTy && !elseTy.isBottom)
                  toBase(ref, elseTy).map { pair =>
                    lmap += TargetType(FalseT) -> toConstr(pair)
                  }
              }
            }
            var rmap: Map[TargetType, TypeConstr] = Map()
            toSymRef(r, rv).map { ref =>
              aux(rty, lty, true, false).map { thenTy =>
                if (rty != thenTy && !thenTy.isBottom)
                  toBase(ref, thenTy).map { pair =>
                    rmap += TargetType(TrueT) -> toConstr(pair)
                  }
              }
              aux(rty, lty, false, false).map { elseTy =>
                if (rty != elseTy && !elseTy.isBottom)
                  toBase(ref, elseTy).map { pair =>
                    rmap += TargetType(FalseT) -> toConstr(pair)
                  }
              }
            }
            val lguard = TypeGuard(lmap)
            val rguard = TypeGuard(rmap)
            val guard = (for {
              dty <- List(TargetType(TrueT), TargetType(FalseT))
              constr = {
                lguard(dty) &&
                rguard(dty)
              }
              newConstr = constr.lift
              if !newConstr.isTop
            } yield dty -> newConstr).toMap
            TypeGuard(guard)
          }
        case EBinary(BOp.Eq, e, EBool(true)) =>
          for {
            v <- transfer(e)
            given AbsState <- get
          } yield v.guard
        case EBinary(BOp.Eq, e, EBool(false)) =>
          getTypeGuard(EUnary(UOp.Not, e))
        case EBinary(BOp.Eq, ERef(ref), r) =>
          for {
            lv <- transfer(ref)
            rv <- transfer(r)
            given AbsState <- get
          } yield {
            val lty = lv.ty
            val rty = rv.ty
            val thenTy = lty && rty
            val elseTy = if (rty.isSingle) lty -- rty else lty
            var guard: Map[TargetType, TypeConstr] = Map()
            toSymRef(ref, lv).map { ref =>
              if (!thenTy.isBottom) toBase(ref, thenTy).map { pair =>
                guard += TargetType(TrueT) -> toConstr(pair)
              }
              if (!elseTy.isBottom) toBase(ref, elseTy).map { pair =>
                guard += TargetType(FalseT) -> toConstr(pair)
              }
            }
            TypeGuard(guard)
          }
        case ETypeCheck(ERef(ref), givenTy) =>
          for {
            lv <- transfer(ref)
            given AbsState <- get
          } yield {
            val lty = lv.ty
            val rty = givenTy.toValue
            val thenTy = lty && rty
            val elseTy = lty -- rty
            var guard: Map[TargetType, TypeConstr] = Map()
            toSymRef(ref, lv).map { ref =>
              if (lty != thenTy)
                if (!thenTy.isBottom) toBase(ref, thenTy).map { p =>
                  guard += TargetType(TrueT) -> toConstr(p)
                }
              if (lty != elseTy)
                if (!elseTy.isBottom) toBase(ref, elseTy).map { p =>
                  guard += TargetType(FalseT) -> toConstr(p)
                }
            }
            TypeGuard(guard)
          }
        case EExists(Field(x: Local, EStr(field))) =>
          val binding = Binding.Exist
          for {
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
            var guard: Map[TargetType, TypeConstr] = Map()
            toSymRef(x, lv).map { ref =>
              if (lty != thenTy)
                if (!thenTy.isBottom) toBase(ref, thenTy).map { p =>
                  guard += TargetType(TrueT) -> toConstr(p)
                }
              if (lty != elseTy)
                if (!elseTy.isBottom) toBase(ref, elseTy).map { p =>
                  guard += TargetType(FalseT) -> toConstr(p)
                }
            }
            TypeGuard(guard)
          }
        // case EExists(Field(x: Local, field)) => TODO
        // case EBinary(BOp.Eq, ETypeOf(l), ETypeOf(r)) => TODO
        case EBinary(BOp.Eq, ETypeOf(ERef(ref)), r) =>
          for {
            lv <- transfer(ref)
            rv <- transfer(r)
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
            var guard: Map[TargetType, TypeConstr] = Map()
            toSymRef(ref, lv).map { ref =>
              if (lty != thenTy)
                if (!thenTy.isBottom) toBase(ref, thenTy).map { p =>
                  guard += TargetType(TrueT) -> toConstr(p)
                }
              if (lty != elseTy)
                if (!elseTy.isBottom) toBase(ref, elseTy).map { p =>
                  guard += TargetType(FalseT) -> toConstr(p)
                }
            }
            TypeGuard(guard)
          }
        case EUnary(UOp.Not, e) =>
          for {
            v <- transfer(e)
            given AbsState <- get
            ty = v.ty
            guard = v.guard
            lt = guard(TargetType(TrueT))
            lf = guard(TargetType(FalseT))
          } yield {
            var guard: Map[TargetType, TypeConstr] = Map()
            guard += TargetType(TrueT) -> lf.lift
            guard += TargetType(FalseT) -> lt.lift
            TypeGuard(guard)
          }
        case EBinary(BOp.Or, l, r) =>
          for {
            lv <- transfer(l)
            st <- get
            given AbsState = st
            lty = lv.ty
            rv <- transfer(r)
            rty = rv.ty
            hasT = lty.bool.contains(true)
            lguard = lv.guard
            lt = lguard(TargetType(TrueT))
            lf = lguard(TargetType(FalseT))
          } yield {
            var guard: Map[TargetType, TypeConstr] = Map()
            val refinedSt = if (lf.isTop) st else refine(lf)(st)
            val (thenConstr, _) = (for {
              rv <- transfer(r)
              rt = rv.guard(TargetType(TrueT))
            } yield if (hasT) lt || rt else rt)(refinedSt)
            if (!thenConstr.isTop)
              guard += TargetType(TrueT) -> thenConstr.lift
            val (elseConstr, _) = (for {
              rv <- transfer(r)
              rf = rv.guard(TargetType(FalseT))
              hasF = lty.bool.contains(false)
            } yield lf && rf)(refinedSt)
            if (!elseConstr.isTop)
              guard += TargetType(FalseT) -> elseConstr.lift
            TypeGuard(guard)
          }
        case EBinary(BOp.And, l, r) =>
          for {
            lv <- transfer(l)
            st <- get
            given AbsState = st
            lty = lv.ty
            rv <- transfer(r)
            rty = rv.ty
            hasF = lty.bool.contains(false)
            lguard = lv.guard
            lt = lguard(TargetType(TrueT))
            lf = lguard(TargetType(FalseT))
          } yield {
            var guard: Map[TargetType, TypeConstr] = Map()
            val refinedSt = if (lt.isTop) st else refine(lt)(st)
            val (thenConstr, _) = (for {
              rv <- transfer(r)
              rt = rv.guard(TargetType(TrueT))
            } yield lt && rt)(refinedSt)
            if (!thenConstr.isTop)
              guard += TargetType(TrueT) -> thenConstr.lift
            val (elseConstr, _) = (for {
              rv <- transfer(r)
              rf = rv.guard(TargetType(FalseT))
            } yield if (hasF) lf || rf else rf)(refinedSt)
            if (!elseConstr.isTop)
              guard += TargetType(FalseT) -> elseConstr.lift
            TypeGuard(guard)
          }
        case EEnum(name) =>
          if TargetType.set.contains(EnumT(name)) then
            get(st => {
              TypeGuard(
                Map(
                  TargetType(EnumT(name)) -> TypeConstr.Top.lift(using st),
                ),
              )
            })
          else TypeGuard.Empty
        case _ => TypeGuard.Empty
      }
    }

    /** transfer function for references */
    def transfer(
      ref: Ref,
    )(using np: NodePoint[Node]): Result[AbsValue] = ref match
      // a precise type of `the active function object` in built-in functions
      case Field(
            Field(Global("EXECUTION_STACK"), EMath(0)),
            EStr("Function"),
          ) if np.func.isBuiltin =>
        AbsValue(RecordT("Constructor"))
      // a precise type for intrinsic objects
      case Field(Field(base, EStr("Intrinsics")), EStr(name)) =>
        for {
          b <- transfer(base)
          given AbsState <- get
          v <-
            if (b.ty <= RealmT) {
              val ty = cfg.init.intrTypes.getOrElse(
                name,
                if (name.startsWith("%Symbol.")) SymbolT else ObjectT,
              )
              pure(AbsValue(ty))
            } else transfer(base)
        } yield v
      case x: Global =>
        for {
          v <- get(_.get(x))
        } yield v
      case x: Local =>
        for {
          v <- get(_.get(x))
        } yield {
          if (v.isSymbolic) v
          else AbsValue(SVar(x), v.guard)
        }
      case field @ Field(base, expr) =>
        for {
          b <- transfer(base)
          p <- transfer(expr)
          given AbsState <- get
          v <- get(_.get(b, p))
        } yield v

    /** transfer function for unary operators */
    def transfer(
      st: AbsState,
      unary: EUnary,
      operand: AbsValue,
    )(using np: NodePoint[Node]): AbsValue = {
      import UOp.*
      given AbsState = st
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
      val NodePoint(func, node, _) = np
      addError(UnaryOpTypeMismatch(UnaryOpPoint(func, node, unary), operandTy))

    /** transfer function for binary operators */
    def transfer(
      st: AbsState,
      binary: EBinary,
      left: AbsValue,
      right: AbsValue,
    )(using np: NodePoint[Node]): AbsValue = {
      import BOp.*
      given AbsState = st
      if (config.checkBinaryOp)
        val (lhsTy, rhsTy) = (left.ty, right.ty)
        binary.bop match
          case Add | Sub | Mul | Pow | Div | Mod | Lt | Equal =>
            checkBinary(binary, lhsTy, rhsTy, Set(ExtMathT, NumberT, BigIntT))
          case LShift | RShift | BAnd | BOr | BXOr =>
            checkBinary(binary, lhsTy, rhsTy, Set(MathT, BigIntT))
          case And | Or | Xor =>
            checkBinary(binary, lhsTy, rhsTy, Set(BoolT))
          case Eq =>
      binary.bop match {
        case BAnd   => left & right
        case BOr    => left | right
        case BXOr   => left ^ right
        case Eq     => left =^= right
        case Equal  => left ==^== right
        case Lt     => left < right
        case And    => left && right
        case Or     => left || right
        case Xor    => left ^^ right
        case Add    => left + right
        case Sub    => left.sub(right)
        case Div    => left / right
        case Mul    => left * right
        case Mod    => left % right
        case Pow    => left ** right
        case LShift => left << right
        case RShift => left >> right
      }
    }

    private def checkBinary(
      binary: EBinary,
      lhsTy: ValueTy,
      rhsTy: ValueTy,
      expectedTys: Set[ValueTy],
    )(using np: NodePoint[Node]): Unit =
      if (!expectedTys.exists(ty => lhsTy <= ty || rhsTy <= ty))
        val NodePoint(func, node, _) = np
        val binaryPoint = BinaryOpPoint(func, node, binary)
        addError(BinaryOpTypeMismatch(binaryPoint, lhsTy, rhsTy))

    /** transfer for variadic operators */
    def transfer(
      st: AbsState,
      vop: VOp,
      vs: List[AbsValue],
    )(using np: NodePoint[Node]): AbsValue =
      given AbsState = st
      vop match
        case VOp.Min =>
          val math = vs.map(_.ty.math).reduce((x, y) => x.min(y))
          val inf = vs.map(_.ty.infinity).reduce(_ || _)
          AbsValue(
            ValueTy(
              math = math,
              infinity = if (math.isBottom) inf else inf && InfinityTy.Neg,
            ),
          )

        case VOp.Max =>
          val math = vs.map(_.ty.math).reduce((x, y) => x.max(y))
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
      st: AbsState,
      mop: MOp,
      vs: List[AbsValue],
    )(using np: NodePoint[Node]): AbsValue = MathTop

    // =========================================================================
    // Implementation for TyChecker
    // =========================================================================
    /** check if the return type can be used */
    lazy val canUseReturnTy: Func => Boolean = cached { func =>
      manualRefiners.contains(func.name) || (
        !func.retTy.isImprec &&
        TargetType.from(func.retTy.ty.toValue).isEmpty
      )
    }

    /** default type guards */
    type Refiner = (Func, List[AbsValue], ValueTy, AbsState) => AbsValue
    val manualRefiners: Map[String, Refiner] = Map(
      "__APPEND_LIST__" -> { (func, vs, retTy, st) =>
        given AbsState = st
        AbsValue(vs(0).ty || vs(1).ty)
      },
      "__FLAT_LIST__" -> { (func, vs, retTy, st) =>
        given AbsState = st
        AbsValue(vs(0).ty.list.elem)
      },
      "__GET_ITEMS__" -> { (func, vs, retTy, st) =>
        given AbsState = st
        val ast = vs(1).ty.toValue.grammarSymbol match
          case Fin(set) => AstT(set.map(_.name))
          case Inf      => AstT
        AbsValue(ListT(ast))
      },
      "__CLAMP__" -> { (func, vs, retTy, st) =>
        given AbsState = st
        val refined =
          if (vs(0).ty.toValue <= (IntT || InfinityT))
            if (vs(1).ty.toValue <= MathT(0)) NonNegIntT
            else IntT
          else retTy
        AbsValue(refined)
      },
      "Completion" -> { (func, vs, retTy, st) =>
        given AbsState = st
        AbsValue(SSym(0))
      },
      "NormalCompletion" -> { (func, vs, retTy, st) =>
        given AbsState = st
        AbsValue(SNormal(SSym(0)))
      },
      "UpdateEmpty" -> { (func, vs, retTy, st) =>
        given AbsState = st
        val record = vs(0).ty.record
        val valueField = record("Value").value
        val updated = record.update(
          "Value",
          vs(1).ty || (valueField -- EnumT("empty")),
          refine = false,
        )
        AbsValue(ValueTy(record = updated))
      },
      "IteratorClose" -> { (func, vs, retTy, st) =>
        given AbsState = st
        // Throw | #1
        AbsValue(vs(1).ty || ThrowT)
      },
      "AsyncIteratorClose" -> { (func, vs, retTy, st) =>
        given AbsState = st
        // Throw | #1
        AbsValue(vs(1).ty || ThrowT)
      },
      "Await" -> { (func, vs, retTy, st) =>
        given AbsState = st
        AbsValue(NormalT(ESValueT) || ThrowT)
      },
      "RequireInternalSlot" -> { (func, vs, retTy, st) =>
        given AbsState = st
        val constr = vs(1).ty.str.getSingle match
          case One(f) =>
            val ty = ValueTy(
              record = ObjectT.record.update(f, Binding.Exist, refine = true),
            )
            TypeConstr(0 -> ty)
          case _ => TypeConstr(0 -> ObjectT)
        val guard = TypeGuard(TargetType(NormalT) -> constr)
        AbsValue(STy(retTy), guard)
      },
      "NewPromiseCapability" -> { (func, vs, retTy, st) =>
        given AbsState = st
        val guard = TypeGuard(
          TargetType(NormalT) -> TypeConstr(0 -> ConstructorT),
        )
        AbsValue(STy(retTy), guard)
      },
      "CreateListFromArrayLike" -> { (func, vs, retTy, st) =>
        given AbsState = st
        AbsValue(
          (for {
            v <- vs.lift(1)
            str = v.ty.list.elem.str
            s <- str match
              case One(s) => Some(s)
              case _      => None
            ty = ValueTy.fromTypeOf(s)
            refined = retTy.toValue && NormalT(ListT(ty))
          } yield refined).getOrElse(retTy),
        )
      },
      "TypedArrayElementType" -> { (func, vs, retTy, st) =>
        AbsValue(
          EnumT(
            "int8",
            "uint8",
            "uint8clamped",
            "int16",
            "uint16",
            "int32",
            "uint32",
            "bigint64",
            "biguint64",
            "float32",
            "float64",
          ),
        )
      },
      "TypedArrayElementSize" -> { (func, vs, retTy, st) =>
        AbsValue(PosIntT)
      },
      // -----------------------------------------------------------------------
      // constratins for object properties and return values of functions
      // -----------------------------------------------------------------------
      "Get" -> { (func, vs, retTy, st) =>
        given AbsState = st
        val ty = vs(1).ty
        AbsValue(ty.getProperty.fold(STy(retTy))(p => SProp(SSym(0), p)))
      },
      "Set" -> { (func, vs, retTy, st) =>
        given AbsState = st
        val ty = vs(1).ty
        val guard = ty.getProperty match
          case Some(p) =>
            val abruptT = ValueTy(
              record = ObjectT.record.update(p, Desc(setExc = true)),
            )
            TypeGuard(
              TargetType(AbruptT) -> TypeConstr(0 -> abruptT),
            )
          case None => TypeGuard()
        AbsValue(STy(retTy), guard)
      },
      "Call" -> { (func, vs, retTy, st) =>
        given AbsState = st
        val ty = vs(1).ty
        AbsValue(SCall(SSym(0)))
      },
      "Construct" -> { (func, vs, retTy, st) =>
        given AbsState = st
        AbsValue(SConstruct(SSym(0)))
      },
    )
  }
}
