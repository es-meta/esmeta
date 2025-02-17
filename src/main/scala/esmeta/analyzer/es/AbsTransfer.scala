package esmeta.analyzer.es

import esmeta.cfg.{util => _, *}
import esmeta.domain.{*, given}
import esmeta.es.*
import esmeta.ir.{Func => _, *}
import esmeta.state.*

trait AbsTransferDecl { analyzer: ESAnalyzer =>

  /** abstract transfer function */
  class AbsTransfer extends AbsTransferLike {

    /** loading monads */
    import monad.*

    /** loading predefined abstract values */
    import AbsValue.*

    // =========================================================================
    // Implementation for General AbsTransfer
    // =========================================================================
    def apply(np: NodePoint[_]): Unit = {
      val st = getResult(np)
      given NodePoint[_] = np
      val NodePoint(func, node, view) = np
      node match {
        case Block(_, insts, next) =>
          val newSt = insts.zipWithIndex.foldLeft(st) {
            case (nextSt, _) if nextSt.isBottom => nextSt
            case (nextSt, (inst, idx))          => transfer(inst, idx)(nextSt)
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
        case Branch(_, kind, c, thenNode, elseNode) =>
          (for { v <- transfer(c); newSt <- get } yield {
            if (v.bool.contains(true))
              thenNode.map(analyzer += getNextNp(np, _) -> newSt)
            if (v.bool.contains(false))
              elseNode.map(analyzer += getNextNp(np, _) -> newSt)
          })(st)
      }
    }

    def apply(rp: ReturnPoint): Unit = {
      var AbsRet(value, st) = getResult(rp)
      for {
        callerNps <- retEdges.get(rp)
        callerNp @ NodePoint(func, call, view) <- callerNps
        nextNode <- callerNp.node.next
      } {
        val callerSt = callInfo(callerNp)
        val nextNp = NodePoint(
          func,
          nextNode,
          nextNode match {
            case br: Branch if br.isLoop => loopEnter(view, br)
            case _                       => view
          },
        )
        analyzer += nextNp -> st.doReturn(
          callerSt,
          call.lhs -> value.opt,
        )
      }
    }

    def transfer(
      inst: NormalInst,
      idx: Int,
    )(using np: NodePoint[_]): Updater = inst match {
      case IExpr(expr) =>
        for {
          v <- transfer(expr)
        } yield v
      case ILet(lhs, expr) =>
        for {
          v <- transfer(expr)
          _ <- modify(_.define(lhs, v))
          st <- get
        } yield ()
      case IAssign(ref, expr) =>
        for {
          rt <- transfer(ref)
          v <- transfer(expr)
          _ <- modify(_.update(rt, v))
        } yield ()
      case IExpand(base, expr)      => ???
      case IDelete(base, expr)      => ???
      case IPush(elem, list, front) => ???
      case IPop(lhs, list, front)   => ???
      case inst @ IReturn(expr) =>
        for {
          v <- transfer(expr)
          _ <- doReturn(inst, v)
          _ <- put(AbsState.Bot)
        } yield ()
      case IAssert(expr) => pure(())
      case IPrint(expr)  => ???
      case INop()        => ???
    }

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
          rt <- transfer(ref)
          v <- transfer(rt)
        } yield v
      case EUnary(uop, expr) =>
        for {
          v <- transfer(expr)
          st <- get
          newV <- transfer(st, uop, v)
        } yield newV
      case EBinary(bop, left, right) =>
        for {
          l <- transfer(left)
          r <- transfer(right)
          st <- get
          newV <- transfer(st, bop, l, r)
        } yield newV
      case EVariadic(vop, exprs) => ???
      case EMathOp(mop, args)    => ???
      case EConvert(cop, expr)   => ???
      case EExists(ref) =>
        for {
          rt <- transfer(ref)
          v <- get(_.exists(rt))
        } yield v
      case ETypeOf(base)             => ???
      case EInstanceOf(base, target) => ???
      case ETypeCheck(base, ty)      => ???
      case ESizeOf(base)             => ???
      case EClo(fname, captured) =>
        cfg.fnameMap.get(fname) match {
          case Some(f) =>
            for { st <- get } yield AbsValue(
              AClo(f, captured.map(x => x -> st.get(x)).toMap),
            )
          case None =>
            for { _ <- put(AbsState.Bot) } yield AbsValue.Bot
        }
      case ECont(fname)                 => ???
      case EDebug(expr)                 => ???
      case ERandom()                    => AbsValue.NumberTop
      case ESyntactic(x, args, idx, ys) => ???
      case ELexical(name, expr)         => ???
      case allocExpr: AllocExpr         => transfer(allocExpr)
      case EMath(n)                     => AbsValue(n)
      case EInfinity(true)              => AbsValue(Double.PositiveInfinity)
      case EInfinity(false)             => AbsValue(Double.NegativeInfinity)
      case ENumber(double)              => AbsValue(double)
      case EBigInt(bigInt)              => AbsValue(bigInt)
      case EStr(str)                    => AbsValue(str)
      case EBool(b)                     => AbsValue(b)
      case EUndef()                     => AbsValue.Undef
      case ENull()                      => AbsValue.Null
      case EEnum(name)                  => AbsValue(Enum(name))
      case ECodeUnit(c)                 => AbsValue(CodeUnit(c))
    }

    def transfer(
      expr: AllocExpr,
    )(using np: NodePoint[Node]): Result[AbsValue] =
      import AddrPart.*
      lazy val part = ASite(expr.asite, np.view)
      expr match {
        case ERecord(tname, fields) =>
          for {
            pairs <- join(fields.map { (f, expr) =>
              for { v <- transfer(expr) } yield (f, v)
            })
            _ <- modify(_.allocRecord(part, tname, pairs))
          } yield AbsValue(part)
        case EMap(ty, pairs)       => ???
        case EList(exprs)          => ???
        case ECopy(obj)            => ???
        case EKeys(map, intSorted) => ???
      }

    /** transfer function for references */
    def transfer(
      ref: Ref,
    )(using np: NodePoint[_]): Result[AbsRefTarget] =
      import AbsRefTarget.*
      ref match {
        case id: Var => AbsId(id)
        case Field(ref, expr) =>
          for {
            rt <- transfer(ref)
            b <- transfer(rt)
            f <- transfer(expr)
          } yield AbsField(b, f)
      }

    /** transfer function for abstract reference targets */
    def transfer(
      rt: AbsRefTarget,
    )(using np: NodePoint[_]): Result[AbsValue] = get(_.get(rt))

    def transfer(
      st: AbsState,
      uop: UOp,
      operand: AbsValue,
    )(using np: NodePoint[Node]): AbsValue = {
      import UOp.*
      given AbsState = st
      uop match
        case Neg   => -operand
        case Not   => !operand
        case BNot  => ~operand
        case Abs   => operand.abs
        case Floor => operand.floor
    }

    def transfer(
      st: AbsState,
      bop: BOp,
      left: AbsValue,
      right: AbsValue,
    )(using np: NodePoint[Node]): AbsValue = ???

    def transfer(
      st: AbsState,
      vop: VOp,
      vs: List[AbsValue],
    )(using np: NodePoint[Node]): AbsValue = ???

    def transfer(
      st: AbsState,
      mop: MOp,
      vs: List[AbsValue],
    )(using np: NodePoint[Node]): AbsValue = ???

    // =========================================================================
    // helper functions
    // =========================================================================
    /** transfer function for call instructions */
    protected def transfer(
      call: Call,
    )(using np: NodePoint[_]): Result[AbsValue] =
      val callerNp = NodePoint(np.func, call, np.view)
      call.callInst match {
        case ICall(_, fexpr, args) =>
          for {
            fv <- transfer(fexpr)
            vs <- join(args.map(transfer))
            st <- get
          } yield {
            for (AClo(func, captured) <- fv.clo)
              doCall(callerNp, func, st, args, vs, captured, func.isMethod)
            // continuation call (unsound for inifinitely many continuations)
            for (ACont(target, captured) <- fv.cont) {
              ???
            }
            AbsValue.Bot
          }
        case ISdoCall(_, base, method, args) => ???
      }

    /** transfer function for branch instructions */
    private def getNextNp(
      fromCp: NodePoint[Node],
      to: Node,
      loopOut: Boolean = false,
    ): NodePoint[Node] = {
      val NodePoint(func, from, view) = fromCp

      // handle loop sensitivity
      val fromView = if (loopOut) analyzer.loopExit(view) else view
      val toView = to match
        case br: Branch if br.isLoop =>
          if (from.isLoopPred) analyzer.loopEnter(view, br)
          else analyzer.loopNext(view)
        case _ => fromView

      // next node point
      NodePoint(func, to, toView)
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
    ): Unit = {
      callInfo += callerNp -> callerSt
      // get locals
      val locals =
        getLocals(callerNp, callee, method, vs) ++
        captured.map { case (k, v) => k -> v.opt }
      // keep caller state to restore it
      for {
        calleeView <- getCalleeViews(callerNp)
        calleeSt = getCalleeState(callerSt, locals)
        calleeNp = NodePoint(callee, callee.entry, calleeView)
      } {
        // add callee to worklist
        analyzer += calleeNp -> calleeSt
        // add return edges from callee to caller
        val rp = ReturnPoint(callee, calleeNp.view)
        val set = retEdges.getOrElse(rp, Set())
        retEdges += rp -> (set + callerNp)
      }
    }

    /** get local variables */
    def getLocals(
      callerNp: NodePoint[Call],
      callee: Func,
      method: Boolean,
      vs: List[AbsValue],
    ): Map[Local, AbsOpt[AbsValue]] = {
      // get parameters
      val params: List[Param] = callee.irFunc.params
      // construct locals with a consideration of absence of arguments
      var locals = Map[Local, AbsOpt[AbsValue]]()
      params.foldLeft(vs) {
        case (Nil, p) =>
          locals += p.lhs -> AbsValue.opt.Bot
          Nil
        case (v :: vs, p) =>
          locals += p.lhs -> v.opt
          vs
      }
      locals
    }

    /** get callee state */
    def getCalleeState(
      callerSt: AbsState,
      locals: Map[Local, AbsOpt[AbsValue]],
    ): AbsState = callerSt.copy(locals = locals)

    /** callee entries */
    def getCalleeViews(callerNp: NodePoint[Call]): List[View] =
      val NodePoint(_, callSite, view) = callerNp
      if (irSens) List(view.copy(calls = callSite :: view.calls, loopDepth = 0))
      else List(view)

    /** update return points */
    def doReturn(
      irReturn: Return,
      v: AbsValue,
    )(using np: NodePoint[Node]): Unit = if (v.nonBottom) {
      val NodePoint(func, node, view) = np
      val st = getResult(np)
      val rp = ReturnPoint(func, getEntryView(view))
      val newRet = AbsRet(v, st.copy(locals = Map()))
      val oldRet = getResult(rp)
      if (!oldRet.value.isBottom && useRepl) Repl.merged = true
      if (newRet !⊑ oldRet)
        rpMap += rp -> (oldRet ⊔ newRet)
        worklist += rp
    }
  }
}
