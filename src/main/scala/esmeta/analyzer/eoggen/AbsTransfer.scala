package esmeta.analyzer.eoggen

import esmeta.cfg.{util => _, *}
import esmeta.ir.{Func => _, util => _, *}
import esmeta.state.*
import esmeta.ty.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import scala.annotation.tailrec
import esmeta.es.builtin.JOB_QUEUE

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
        case br @ Branch(_, kind, c, _, thenNode, elseNode) => ???
    // import RefinementTarget.*
    // import RefinementKind.*
    // (for { v <- transfer(c); newSt <- get } yield {
    //   if (v.ty.bool.contains(true))
    //     val rst = refine(c, v, true)(newSt)
    //     val pred = v.guard.get(RefinementKind(TrueT))
    //     if (detail) logRefined(BranchTarget(br, true), pred, newSt, rst)
    //     thenNode.map(analyzer += getNextNp(np, _) -> rst)
    //   if (v.ty.bool.contains(false))
    //     val rst = refine(c, v, false)(newSt)
    //     val pred = v.guard.get(RefinementKind(FalseT))
    //     if (detail) logRefined(BranchTarget(br, false), pred, newSt, rst)
    //     elseNode.map(analyzer += getNextNp(np, _) -> rst)
    // })(st)

    /** get next node point */
    def getNextNp(fromCp: NodePoint[Node], to: Node): NodePoint[Node] =
      fromCp.copy(node = to)

    /** transfer function for return points */
    def apply(rp: ReturnPoint): Unit = ???

    /** get after call node point */
    def getAfterCallNp(callerNp: NodePoint[Call]): Option[NodePoint[Node]] =
      callerNp.node.next.map(nextNode => callerNp.copy(node = nextNode))

    /** transfer function for call instructions */
    def transfer(
      call: Call,
    )(using np: NodePoint[_]): Result[AbsValue] = {
      val callerNp = NodePoint(np.func, call, np.view)
      call.callInst match {
        case ICall(_, fexpr, args) => ???
        case ISdoCall(_, base, method, args) =>
          for {
            bv <- transfer(base)
            vs <- join(args.map(transfer))
            st <- get
            given AbsState = st
          } yield {
            var newV: AbsValue = AbsValue.Bot
            // lexical sdo
            newV ⊔= bv.getLexical(method)
            // syntactic sdo
            bv.getSdo(method) match
              case Zero =>
              case One((ast, sdo)) =>
                doCall(
                  callerNp,
                  sdo,
                  st,
                  base :: args,
                  ast :: vs,
                  method = true,
                )
              case Many => ???
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
    ): Map[Local, AbsValue] = ???

    /** transfer function for normal instructions */
    def transfer(
      inst: NormalInst,
    )(using np: NodePoint[_]): Updater = ???

    /** update return points */
    def doReturn(
      irReturn: Return,
      v: AbsValue,
    )(using np: NodePoint[Node]): Unit = ???

    /** transfer function for expressions */
    def transfer(
      expr: Expr,
    )(using np: NodePoint[Node]): Result[AbsValue] = ???

    /** transfer function for references */
    def transfer(
      ref: Ref,
    )(using np: NodePoint[Node]): Result[AbsValue] = ???

    /** transfer function for unary operators */
    def transfer(
      st: AbsState,
      unary: EUnary,
      operand: AbsValue,
    )(using np: NodePoint[Node]): AbsValue = ???

    /** transfer function for binary operators */
    def transfer(
      st: AbsState,
      binary: EBinary,
      left: AbsValue,
      right: AbsValue,
    )(using np: NodePoint[Node]): AbsValue = ???

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
