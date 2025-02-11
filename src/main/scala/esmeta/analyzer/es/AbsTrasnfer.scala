package esmeta.analyzer.es

import esmeta.ir.{Func => _, *}
import esmeta.cfg.{util => _, *}

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
        case Block(_, insts, next) => ???
        case call: Call =>
          val (_, newSt) = (for {
            v <- transfer(call)
            _ <-
              if (v.isBottom) put(AbsState.Bot)
              else modify(_.define(call.lhs, v))
          } yield ())(st)
          call.next.foreach(to => analyzer += getNextNp(np, to) -> newSt)
        case Branch(_, kind, c, thenNode, elseNode) => ???
      }
    }

    def apply(rp: ReturnPoint): Unit = ???

    def transfer(
      inst: NormalInst,
    )(using np: NodePoint[_]): Updater = ???

    def transfer(
      expr: Expr,
    )(using np: NodePoint[Node]): Result[AbsValue] = expr match {
      case EParse(code, rule)                       => ???
      case EGrammarSymbol(name, params)             => ???
      case ESourceText(expr)                        => ???
      case EYet(msg)                                => ???
      case EContains(list, expr)                    => ???
      case ESubstring(expr, from, to)               => ???
      case ETrim(expr, isStarting)                  => ???
      case ERef(ref)                                => ???
      case EUnary(uop, expr)                        => ???
      case EBinary(bop, left, right)                => ???
      case EVariadic(vop, exprs)                    => ???
      case EMathOp(mop, args)                       => ???
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
      case EMath(n)                                 => ???
      case EInfinity(pos)                           => ???
      case ENumber(double)                          => ???
      case EBigInt(bigInt)                          => ???
      case EStr(str)                                => ???
      case EBool(b)                                 => ???
      case EUndef()                                 => ???
      case ENull()                                  => ???
      case EEnum(name)                              => ???
      case ECodeUnit(c)                             => ???
    }

    def transfer(
      st: AbsState,
      unary: EUnary,
      operand: AbsValue,
    )(using np: NodePoint[Node]): AbsValue = ???

    def transfer(
      st: AbsState,
      binary: EBinary,
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
            as <- join(args.map(transfer))
            st <- get
          } yield {
            for (AClo(func, captured) <- fv.clo)
              doCall(callerNp, st, func, as, captured)
            // // continuation call (unsound for inifinitely many continuations)
            // for (ACont(target, captured) <- fv.cont) {
            //   val as0 =
            //     as.map(v => if (cp.func.isReturnComp) v.wrapCompletion else v)
            //   val newLocals = getLocals(
            //     CallPoint(callerNp, target),
            //     as0,
            //     cont = true,
            //     method = false,
            //   ) ++ captured
            //   sem += target -> st.copied(locals = newLocals)
            // }
            // AbsValue.Bot
            ???
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
      callerSt: AbsState,
      calleeFunc: Func,
      args: List[AbsValue],
      captured: Map[Name, AbsValue] = Map(),
      method: Boolean = false,
    ): Unit = ???
  }
}
