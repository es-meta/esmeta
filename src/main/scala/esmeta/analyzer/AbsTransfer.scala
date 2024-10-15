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

trait AbsTransferDecl { self: Analyzer =>

  /** abstract transfer function */
  trait AbsTransfer {

    /** loading monads */
    import AbsState.monad.*

    /** given instance for CFG */
    given CFG = cfg

    /** fixpiont computation */
    @tailrec
    final def fixpoint: Unit = sem.worklist.next match
      case Some(cp) =>
        // set the current control point
        sem.curCp = Some(cp)
        // count how many visited for each control point
        sem.count(cp)
        // increase iteration number
        sem.iter += 1
        // check time limit
        if (sem.iter % checkPeriod == 0) timeLimit.map(limit => {
          val duration = (System.currentTimeMillis - sem.startTime) / 1000
          if (duration > limit) exploded("timeout")
        })
        // text-based debugging
        if (debugMode) println(s"${cp.func.name}:$cp")
        // run REPL
        if (useRepl) Repl(this, cp)
        // abstract transfer for the current control point
        else apply(cp)
        // keep going
        fixpoint
      case None =>
        // set the current control point
        sem.curCp = None
        // finalize REPL
        if (useRepl) Repl.finished

    /** transfer function for control points */
    def apply(cp: ControlPoint): Unit = cp match
      case (np: NodePoint[_]) => this(np)
      case (rp: ReturnPoint)  => this(rp)

    /** transfer function for node points */
    def apply(np: NodePoint[_]): Unit =
      // record current control point for alarm
      given NodePoint[_] = np
      val st = sem(np)
      val NodePoint(func, node, view) = np
      node match
        case Block(_, insts, next) =>
          val newSt = insts.foldLeft(st) {
            case (nextSt, _) if nextSt.isBottom => nextSt
            case (nextSt, inst)                 => transfer(inst)(nextSt)
          }
          next.foreach(to => sem += getNextNp(np, to) -> newSt)
        case call: Call =>
          val (_, newSt) = (for {
            v <- transfer(call)
            _ <-
              if (v.isBottom) put(AbsState.Bot)
              else modify(_.define(call.lhs, v))
          } yield ())(st)
          call.next.foreach(to => sem += getNextNp(np, to) -> newSt)
        case br @ Branch(_, kind, c, thenNode, elseNode) =>
          (for { v <- transfer(c); newSt <- get } yield {
            if (AVT ⊑ v)
              thenNode.map(sem += getNextNp(np, _) -> refine(c, true)(newSt))
            if (AVF ⊑ v)
              elseNode.map(sem += getNextNp(np, _) -> refine(c, false)(newSt))
          })(st)

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
      for {
        callerNp <- sem.getRetEdges(rp)
        nextNp <- getAfterCallNp(callerNp)
      } {
        val callerSt = sem.callInfo(callerNp)
        sem += nextNp -> st.doReturn(callerSt, callerNp.node.lhs, value)
      }
    }

    // transfer function for expressions
    def apply(expr: Expr)(using np: NodePoint[Node]): AbsValue =
      val st = sem.getState(np)
      transfer(expr)(st)._1

    /** get syntax-directed operation (SDO) */
    val getSdo =
      cached[(Ast, String), Option[(Ast, Func)]](
        _.getSdo(_)(using cfg.spec, cfg.fnameMap),
      )

    /** transfer function for normal instructions */
    def transfer(inst: NormalInst)(using np: NodePoint[_]): Updater = inst match
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
      case IAssign(ref, expr) =>
        for {
          rv <- transfer(ref)
          v <- transfer(expr)
          _ <- modify(_.update(rv, v))
        } yield ()
      case IExpand(base, expr) =>
        for {
          b <- transfer(base)
          v <- transfer(expr)
          _ <- modify(st => st.expand(st.get(b), v))
        } yield ()
      case IDelete(base, expr) =>
        for {
          b <- transfer(base)
          v <- transfer(expr)
          _ <- modify(st => st.delete(st.get(b), v))
        } yield ()
      case IPush(expr, list, front) =>
        for {
          l <- transfer(list)
          v <- transfer(expr)
          _ <- modify(_.push(l, v, front))
        } yield ()
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
          _ <- modify(refine(expr, true))
          _ <- if (v ⊑ AVF) put(AbsState.Bot) else pure(())
        } yield ()
      case IPrint(expr) => st => st /* skip */
      case INop()       => st => st /* skip */

    /** transfer function for call instructions */
    def transfer(call: Call)(using np: NodePoint[_]): Result[AbsValue] =
      val callerNp = NodePoint(np.func, call, np.view)
      call.callInst match {
        case ICall(_, fexpr, args) =>
          for {
            fv <- transfer(fexpr)
            vs <- join(args.map(transfer))
            st <- get
          } yield {
            // closure call (XXX: unsound for inifinitely many closures)
            for (AClo(f, captured) <- fv.clo.toIterable(stop = false))
              val callPoint = CallPoint(callerNp, f)
              doCall(callPoint, st, args, vs, captured, f.isMethod)
            // continuation call (XXX: unsound for inifinitely many continuations)
            for (ACont(tgt, captured) <- fv.cont.toIterable(stop = false))
              val f = tgt.func
              val callPoint = CallPoint(callerNp, f)
              doCall(callPoint, st, args, vs, captured, f.isMethod, Some(tgt))
            if (fv.clo.isTop || fv.cont.isTop) AbsValue.Top
            else AbsValue.Bot
          }
        case ISdoCall(_, base, method, args) =>
          for {
            bv <- transfer(base)
            vs <- join(args.map(transfer))
            st <- get
          } yield {
            var newV: AbsValue = AbsValue.Bot
            bv.getSingle match
              case One(AstValue(syn: Syntactic)) =>
                getSdo((syn, method)) match
                  case Some((ast, sdo)) =>
                    val callPoint = CallPoint(callerNp, sdo)
                    val astV = AbsValue(ast)
                    doCall(callPoint, st, args, astV :: vs, method = true)
                  case None => error("invalid sdo")
              case One(AstValue(lex: Lexical)) =>
                newV ⊔= AbsValue(Interpreter.eval(lex, method))
              case Many =>
                // lexical sdo
                newV ⊔= bv.getLexical(method)

                // syntactic sdo
                for ((sdo, ast) <- bv.getSdo(method))
                  val callPoint = CallPoint(callerNp, sdo)
                  doCall(callPoint, st, args, ast :: vs, method = true)
              case _ => /* do nothing */
            newV
          }
      }

    /** transfer function for expressions */
    def transfer(expr: Expr)(using np: NodePoint[Node]): Result[AbsValue] =
      expr match {
        case EParse(code, rule) =>
          for {
            c <- transfer(code)
            r <- transfer(rule)
          } yield c.parse(r)
        case EGrammarSymbol(name, params) =>
          AbsValue(GrammarSymbol(name, params))
        case ESourceText(expr) =>
          for { v <- transfer(expr) } yield v.sourceText
        case EYet(msg) =>
          if (yetThrow) notSupported(msg)
          else AbsValue.Bot
        case EContains(list, elem) =>
          for {
            l <- transfer(list)
            v <- transfer(elem)
            st <- get
          } yield st.contains(l, v)
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
            rv <- transfer(ref)
            v <- transfer(rv)
          } yield v
        case unary @ EUnary(_, expr) =>
          for {
            v <- transfer(expr)
            v0 <- get(transfer(_, unary, v))
          } yield v0
        case binary @ EBinary(BOp.And, left, right) =>
          shortCircuit(binary, left, right)
        case binary @ EBinary(BOp.Or, left, right) =>
          shortCircuit(binary, left, right)
        case binary @ EBinary(_, left, right) =>
          for {
            lv <- transfer(left)
            rv <- transfer(right)
            v <- get(transfer(_, binary, lv, rv))
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
              case ToStr(None)        => pure(AbsValue(Math(10)))
              case _                  => pure(AbsValue.Bot)
          } yield v.convertTo(cop, r)
        case EExists(ref) =>
          for {
            rv <- transfer(ref)
            v <- get(_.exists(rv))
          } yield v
        case ETypeOf(base) =>
          for {
            v <- transfer(base)
            st <- get
          } yield v.typeOf(st)
        case EInstanceOf(expr, target) =>
          for {
            v <- transfer(expr)
            t <- transfer(target)
          } yield v.instanceOf(t)
        case ETypeCheck(expr, ty) =>
          for {
            v <- transfer(expr)
            st <- get
          } yield v.typeCheck(ty.ty, st)
        case ESizeOf(expr) =>
          for {
            v <- transfer(expr)
            st <- get
          } yield v.sizeOf(st)
        case EClo(fname, cap) =>
          cfg.fnameMap.get(fname) match {
            case Some(f) =>
              for {
                st <- get
                captured = cap.map(x => x -> st.get(x)).toMap
              } yield AbsValue(AClo(f, captured))
            case None =>
              for { _ <- put(AbsState.Bot) } yield AbsValue.Bot
          }
        case ECont(fname) =>
          for {
            st <- get
            func = cfg.fnameMap(fname)
            target = NodePoint(func, func.entry, np.view)
            captured = st.locals.collect { case (x: Name, av) => x -> av }
            // return edges for resumed evaluation
            currRp = ReturnPoint(np.func, np.view)
            contRp = ReturnPoint(func, np.view)
            _ = sem.retEdges += (contRp -> sem.retEdges.getOrElse(
              currRp,
              Set(),
            ))
          } yield AbsValue(ACont(target, captured))
        case EDebug(expr) =>
          for {
            v <- transfer(expr)
            st <- get
            _ = debug(s"[[ $expr @ $np ]]($st) = $v")
          } yield v
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
        case e @ ERecord(tname, fields) =>
          val asite = AllocSite(e.asite, np.view)
          for {
            pairs <- join(fields.map {
              case (f, expr) =>
                for {
                  v <- transfer(expr)
                } yield (f, v)
            })
            lv <- id(_.allocRecord(tname, pairs)(asite))
          } yield lv
        case e @ EMap(_, pairs) =>
          val asite = AllocSite(e.asite, np.view)
          for {
            ps <- join(pairs.map {
              case (k, v) =>
                for {
                  key <- transfer(k)
                  value <- transfer(v)
                } yield (key, value)
            })
            lv <- id(_.allocMap(ps)(asite))
          } yield lv
        case e @ EList(exprs) =>
          val asite = AllocSite(e.asite, np.view)
          for {
            vs <- join(exprs.map(transfer))
            lv <- id(_.allocList(vs)(asite))
          } yield lv
        case e @ ECopy(obj) =>
          val asite = AllocSite(e.asite, np.view)
          for {
            v <- transfer(obj)
            lv <- id(_.copy(v)(asite))
          } yield lv
        case e @ EKeys(map, intSorted) =>
          val asite = AllocSite(e.asite, np.view)
          for {
            v <- transfer(map)
            lv <- id(_.keys(v, intSorted)(asite))
          } yield lv
        case EMath(n)              => AbsValue(Math(n))
        case EInfinity(pos)        => AbsValue(Infinity(pos))
        case ENumber(n) if n.isNaN => AbsValue(Double.NaN)
        case ENumber(n)            => AbsValue(n)
        case EBigInt(n)            => AbsValue(BigInt(n))
        case EStr(str)             => AbsValue(Str(str))
        case EBool(b)              => AbsValue(Bool(b))
        case EUndef()              => AbsValue(Undef)
        case ENull()               => AbsValue(Null)
        case EEnum(name)           => AbsValue(Enum(name))
        case ECodeUnit(c)          => AbsValue(CodeUnit(c))
      }

    /** transfer function for references */
    def transfer(ref: Ref)(using np: NodePoint[Node]): Result[AbsRefTarget] =
      ref match
        case x: Var => AbsVarTarget(x)
        case field @ Field(base, expr) =>
          for {
            rv <- transfer(base)
            b <- transfer(rv)
            p <- transfer(expr)
          } yield AbsFieldTarget(b, p)

    /** transfer function for reference values */
    def transfer(rt: AbsRefTarget)(using
      np: NodePoint[Node],
    ): Result[AbsValue] =
      for { v <- get(_.get(rt)) } yield v

    /** transfer function for unary operators */
    def transfer(
      st: AbsState,
      unary: EUnary,
      operand: AbsValue,
    )(using np: NodePoint[Node]): AbsValue =
      import UOp.*
      val uop = unary.uop
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
      binary: EBinary,
      left: AbsValue,
      right: AbsValue,
    )(using np: NodePoint[Node]): AbsValue =
      import BOp.*
      val bop = binary.bop
      (left.getSingle, right.getSingle) match {
        case (Zero, _) | (_, Zero) => AbsValue.Bot
        case (One(l: SimpleValue), One(r: SimpleValue)) =>
          optional(AbsValue(Interpreter.eval(bop, l, r)))
            .getOrElse(AbsValue.Bot)
        case (One(Math(l)), One(Math(r))) =>
          optional(AbsValue(Interpreter.eval(bop, Math(l), Math(r))))
            .getOrElse(AbsValue.Bot)
        case (One(lpart: Part), One(rpart: Part))
            if bop == Eq || bop == Equal =>
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
    def transfer(vop: VOp, vs: List[AbsValue])(using
      np: NodePoint[Node],
    ): AbsValue =
      AbsValue.vopTransfer(vop, vs)

    /** transfer for mathematical operators */
    def transfer(mop: MOp, vs: List[AbsValue])(using
      np: NodePoint[Node],
    ): AbsValue =
      AbsValue.mopTransfer(mop, vs)

    /** handle calls */
    def doCall(
      callPoint: CallPoint,
      callerSt: AbsState,
      args: List[Expr],
      vs: List[AbsValue],
      captured: Map[Name, AbsValue] = Map(),
      method: Boolean = false,
      contTarget: Option[NodePoint[Node]] = None,
    ): Unit =
      val CallPoint(callerNp, callee) = callPoint
      // get locals
      val locals = getLocals(callPoint, method, vs) ++ captured
      // keep caller state to restore it
      contTarget match
        case Some(target) =>
          sem += target -> callerSt.setLocal(locals.toMap)
        case None =>
          sem.callInfo += callerNp -> callerSt
          for {
            (calleeView, newLocals) <- getCalleeEntries(callerNp, locals)
            calleeSt = callerSt.setLocal(newLocals.toMap)
            calleeNp = NodePoint(callee, callee.entry, calleeView)
          } {
            // add callee to worklist
            sem += calleeNp -> calleeSt
            // add return edges from callee to caller
            val rp = ReturnPoint(callee, calleeNp.view)
            val set = sem.getRetEdges(rp)
            sem.retEdges += rp -> (set + callerNp)
            // propagate callee analysis result
            propagate(rp, callerNp)
          }

    /** get after call node point */
    def getAfterCallNp(callerNp: NodePoint[Call]): Option[NodePoint[Node]] =
      val NodePoint(func, call, view) = callerNp
      callerNp.node.next.map { nextNode =>
        NodePoint(
          func,
          nextNode,
          nextNode match
            case br: Branch if br.isLoop => sem.loopEnter(view, br)
            case _                       => view,
        )
      }

    /** propagate callee analysis result */
    def propagate(rp: ReturnPoint, callerNp: NodePoint[Call]): Unit =
      val retT = sem(rp)
      if (!retT.isBottom) sem.worklist += rp

    // return specific value
    def doReturn(
      irReturn: Return,
      v: AbsValue,
    )(using returnNp: NodePoint[Node]): Result[Unit] = for {
      st <- get
      ret = AbsRet(v, st.setLocal(Map()))
      irp = InternalReturnPoint(returnNp, irReturn)
      _ = doReturn(irp, ret)
    } yield ()

    /** update return points */
    def doReturn(irp: InternalReturnPoint, newRet: AbsRet): Unit =
      val InternalReturnPoint(NodePoint(func, _, view), _) = irp
      val retRp = ReturnPoint(func, sem.getEntryView(view))
      if (!newRet.value.isBottom)
        val oldRet = sem(retRp)
        if (!oldRet.isBottom && useRepl) Repl.merged = true
        if (newRet !⊑ oldRet)
          sem.rpMap += retRp -> (oldRet ⊔ newRet)
          sem.worklist += retRp

    // short circuit evaluation
    def shortCircuit(
      binary: EBinary,
      left: Expr,
      right: Expr,
    )(using np: NodePoint[Node]): Result[AbsValue] = for {
      l <- transfer(left)
      st <- get
      v <- binary.bop match {
        case BOp.And =>
          l.bool.cond(AbsState)(AbsValue)(
            thenBranch = transfer(right).eval(refine(left, true)(st)),
            elseBranch = AVF,
          )
        case BOp.Or =>
          l.bool.cond(AbsState)(AbsValue)(
            thenBranch = AVT,
            elseBranch = transfer(right).eval(refine(left, false)(st)),
          )
        case _ =>
          for {
            r <- transfer(right)
            v <- get(transfer(_, binary, l, r))
          } yield v
      }
    } yield v

    /** callee entries */
    def getCalleeEntries(
      callerNp: NodePoint[Call],
      locals: List[(Local, AbsValue)],
    ): List[(View, List[(Local, AbsValue)])] =
      val NodePoint(_, callSite, callerView) = callerNp
      lazy val calls = callSite :: callerView.calls
      val view =
        // handle ir callsite sensitivity
        if (irSens) callerView.copy(calls = calls, intraLoopDepth = 0)
        else callerView
      List(view -> locals)

    /** get local variables */
    def getLocals(
      callPoint: CallPoint,
      method: Boolean,
      vs: List[AbsValue],
    ): List[(Local, AbsValue)] =
      val CallPoint(callerNp, callee) = callPoint
      // get parameters
      val params: List[Param] = callee.irFunc.params
      // full arguments with optional parameters
      // construct local type environment
      (for {
        ((param, arg), idx) <- (params zip vs).zipWithIndex
      } yield param.lhs -> assignArg(callPoint, method, idx, param, arg))

    /** assign argument to parameter */
    def assignArg(
      callPoint: CallPoint,
      method: Boolean,
      idx: Int,
      param: Param,
      arg: AbsValue,
    ): AbsValue = arg

    /** refine condition */
    def refine(
      cond: Expr,
      positive: Boolean,
    )(using np: NodePoint[_]): Updater = st => st
  }
}
