package esmeta.analyzer.eoggen

import esmeta.cfg.{util => _, *}
import esmeta.es.ChainResult
import esmeta.ir.{Func => _, util => _, *}
import esmeta.state.*
import esmeta.ty.*
import esmeta.interpreter.Interpreter
import esmeta.util.*
import esmeta.util.BaseUtils.*
import scala.annotation.tailrec

trait AbsTransferDecl { analyzer: EOGGenerator =>

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
          val newSt = insts.foldLeft(st) { (nst, inst) => transfer(inst)(nst) }
          next.foreach(to => analyzer += getNextNp(np, to) -> newSt)
        case call: Call =>
          val (_, newSt) = (for {
            v <- transfer(call)
            _ <-
              if (v.isBottom) put(AbsState.Bot)
              else modify(_.define(call.lhs, v))
          } yield ())(st)
          call.next.foreach(to => analyzer += getNextNp(np, to) -> newSt)
        case br @ Branch(_, kind, cond, _, thenNode, elseNode) =>
          (for { v <- transfer(cond) } yield {
            if (v.value.contains(Bool(true)))
              thenNode.map(analyzer += getNextNp(np, _) -> st)
            if (v.value.contains(Bool(false)))
              elseNode.map(analyzer += getNextNp(np, _) -> st)
          })(st)

    /** get next node point */
    def getNextNp(fromCp: NodePoint[Node], to: Node): NodePoint[Node] =
      fromCp.copy(node = to)

    /** transfer function for return points */
    def apply(rp: ReturnPoint): Unit = {
      var AbsRet(value) = getResult(rp)
      for {
        callerNps <- retEdges.get(rp)
        callerNp <- callerNps
        nextNp <- getAfterCallNp(callerNp)
      } {
        given callerSt: AbsState = callInfo(callerNp)
        val newV = value
        val nextSt = callerSt.define(callerNp.node.lhs, newV)
        analyzer += nextNp -> nextSt
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
            vs <- join(args.map(transfer))
            st <- get
          } yield {
            var newV: AbsValue = AbsValue.Bot
            fv.value match
              case Zero          =>
              case One(clo: Clo) =>
                // check if contains ast
                if (
                  vs.exists {
                    _.value match
                      case One(_: AstValue) => true
                      case _                => false
                  }
                ) doCall(callerNp, clo.func, st, args, vs, method = false)
                else newV = AbsValue.Top
              case _ => newV = AbsValue.Top
            newV
          }
        case ISdoCall(_, base, method, args) =>
          for {
            bv <- transfer(base)
            vs <- join(args.map(transfer))
            st <- get
            given AbsState = st
          } yield {
            import ChainResult.*

            var newV: AbsValue = AbsValue.Bot
            // lexical sdo
            newV ⊔= bv.getLexical(method)
            // syntactic sdo
            bv.getSdo(method) match
              case Zero | One(NotFound) =>
              case One(Found(ast, sdo)) =>
                doCall(
                  callerNp,
                  sdo,
                  st,
                  base :: args,
                  AbsValue(ast) :: vs,
                  method = true,
                )
              case One(MeetHole(hole)) =>
                holeSdoInfo += callerNp -> hole
                newV = AbsValue.Top
              case Many => newV = AbsValue.Top
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
    ): Unit = {
      given AbsState = callerSt
      callInfo += callerNp -> callerSt
      val NodePoint(caller, call, callerView) = callerNp
      // get locals
      val locals = getLocals(callerNp, callee, method, vs) ++ captured
      val calleeSt = AbsState(locals)
      val calleeView = callerView + call
      val calleeNp = NodePoint(callee, callee.entry, calleeView)
      // add callee to worklist
      analyzer += calleeNp -> calleeSt
      // add return edges from callee to caller
      val rp = ReturnPoint(callee, calleeNp.view)
      val set = retEdges.getOrElse(rp, Set())
      retEdges += rp -> (set + callerNp)
    }

    /** get local variables */
    def getLocals(
      callerNp: NodePoint[Call],
      callee: Func,
      method: Boolean,
      vs: List[AbsValue],
    ): Map[Local, AbsValue] =
      val params = callee.irFunc.params
      (for { (param, arg) <- (params zip vs) } yield param.lhs -> arg).toMap

    /** transfer function for normal instructions */
    def transfer(
      inst: NormalInst,
    )(using np: NodePoint[_]): Updater = inst match {
      case ILet(x, expr) =>
        for {
          v <- transfer(expr)
          _ <- modify(_.define(x, v))
          st <- get
        } yield ()
      case IAssign(x: Local, expr) =>
        for {
          v <- transfer(expr)
          _ <- modify(_.define(x, v))
          st <- get
        } yield ()
      case inst @ IReturn(expr) =>
        for {
          v <- transfer(expr)
          _ <- doReturn(inst, v)
          _ <- put(AbsState.Bot)
        } yield ()
      case _ => st => st
    }

    /** update return points */
    def doReturn(
      irReturn: Return,
      newV: AbsValue,
    )(using np: NodePoint[Node]): Unit =
      val NodePoint(func, node, view) = np
      val rp = ReturnPoint(func, view)
      val AbsRet(oldV) = getResult(rp)
      val entryNp = NodePoint(func, func.entry, view)
      val entrySt = getResult(entryNp)
      given AbsState = entrySt
      if (!oldV.isBottom && useRepl) Repl.merged = true
      if (newV !⊑ oldV)
        val v = (oldV ⊔ newV)
        rpMap += rp -> AbsRet(v)
        worklist += rp

    /** transfer function for expressions */
    def transfer(
      expr: Expr,
    )(using np: NodePoint[Node]): Result[AbsValue] = expr match {
      case EParse(code, rule) =>
        for {
          c <- transfer(code)
          r <- transfer(rule)
          given AbsState <- get
        } yield {
          (c.getSingle, r.getSingle) match
            case (One(c), One(r)) =>
              Interpreter.parse(code, c, rule, r) match
                case Some(ast) => AbsValue(ast)
                case None      => AbsValue.Bot
            case (Zero, _) | (_, Zero) => AbsValue.Bot
            case _                     => AbsValue.Top
        }
      case EGrammarSymbol(name, params) => AbsValue(GrammarSymbol(name, params))
      case ESourceText(expr)            => ???
      case EYet(msg)                    => ???
      case EContains(list, elem)        => ???
      case ESubstring(expr, from, None) => ???
      case ESubstring(expr, from, Some(to)) => ???
      case ETrim(expr, isStarting)          => ???
      case ERef(ref) =>
        for {
          v <- transfer(ref)
        } yield v
      case unary @ EUnary(op, expr) =>
        for {
          v <- transfer(expr)
          st <- get
          u <- transfer(st, unary, v)
        } yield u
      case binary @ EBinary(_, left, right) =>
        for {
          lv <- transfer(left)
          rv <- transfer(right)
          st <- get
          v <- transfer(st, binary, lv, rv)
        } yield v
      case EVariadic(vop, exprs)     => ???
      case EMathOp(mop, exprs)       => ???
      case EConvert(cop, expr)       => ???
      case EExists(ref)              => ???
      case ETypeOf(base)             => ???
      case EInstanceOf(expr, target) => ???
      case ETypeCheck(expr, ty) =>
        for {
          v <- transfer(expr)
          bool = v.value match
            case Zero => AbsValue.Bot
            case One(elem) =>
              ty.ty
                .containsNonAddr(elem)
                .map(b => AbsValue(Bool(b)))
                .getOrElse(AbsValue.Top)
            case Many => AbsValue.Top
        } yield bool
      case ESizeOf(expr) => ???
      case EClo(fname, captured) =>
        captured match
          case Nil => AbsValue(Clo(cfg.fnameMap(fname), Map.empty))
          case _   => ??? // what
      case ECont(fname)                   => ???
      case EDebug(expr)                   => ???
      case ERandom()                      => ???
      case ESyntactic(name, _, rhsIdx, _) => ???
      case ELexical(name, expr)           => ???
      case ERecord(tname, fields)         => ???
      case EMap((kty, vty), _)            => ???
      case EList(exprs)                   => ???
      case ECopy(obj)                     => ???
      case EKeys(map, intSorted)          => ???
      case EMath(n)                       => AbsValue(Math(n))
      case EInfinity(pos)                 => AbsValue(Infinity(pos))
      case ENumber(n)                     => AbsValue(Number(n))
      case EBigInt(n)                     => AbsValue(BigInt(n))
      case EStr(str)                      => AbsValue(Str(str))
      case EBool(b)                       => AbsValue(Bool(b))
      case EUndef()                       => AbsValue(Undef)
      case ENull()                        => AbsValue(Null)
      case EEnum(name)                    => AbsValue(Enum(name))
      case ECodeUnit(c)                   => AbsValue(CodeUnit(c))
    }

    /** transfer function for references */
    def transfer(
      ref: Ref,
    )(using np: NodePoint[Node]): Result[AbsValue] = ref match {
      case x: Var =>
        for {
          v <- get(_.get(x))
        } yield v
      case Field(base, expr) =>
        for {
          b <- transfer(base)
          p <- transfer(expr)
          given AbsState <- get
          v <- get(_.get(b, p))
        } yield v
    }

    /** transfer function for unary operators */
    def transfer(
      st: AbsState,
      unary: EUnary,
      operand: AbsValue,
    )(using np: NodePoint[Node]): AbsValue =
      (unary.uop, operand.value) match
        case (_, Many)     => AbsValue.Top
        case (_, Zero)     => AbsValue.Bot
        case (uop, One(v)) => AbsValue(Interpreter.eval(uop, v))

    /** transfer function for binary operators */
    def transfer(
      st: AbsState,
      binary: EBinary,
      left: AbsValue,
      right: AbsValue,
    )(using np: NodePoint[Node]): AbsValue =
      (binary.bop, left.value, right.value) match
        case (_, Many, _) | (_, _, Many) => AbsValue.Top
        case (_, Zero, _) | (_, _, Zero) => AbsValue.Bot
        // TODO short circuit?
        case (bop, One(l), One(r)) => AbsValue(Interpreter.eval(bop, l, r))

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
  }
}
