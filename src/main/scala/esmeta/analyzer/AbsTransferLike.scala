package esmeta.analyzer

import esmeta.cfg.{util => _, *}
import esmeta.ir.{Func => _, util => _, *}
import scala.annotation.tailrec

trait AbsTransferLikeDecl { self: Analyzer =>

  /** abstract transfer function */
  trait AbsTransferLike {

    /** loading monads */
    import monad.*

    /** fixpiont computation */
    @tailrec
    final def fixpoint: Unit = worklist.next match
      case Some(cp) =>
        // set the current control point
        curCp = Some(cp)
        // count how many visited for each control point
        count(cp)
        // increase iteration number
        iter += 1
        // check time limit
        if (iter % checkPeriod == 0) timeLimit.map(limit => {
          val duration = (System.currentTimeMillis - startTime) / 1000
          if (duration > limit) exploded("timeout")
        })
        // text-based debugging
        if (debugMode) println(s"${cp.func.name}:$cp")
        // run REPL
        if (useRepl) Repl(cp)
        // abstract transfer for the current control point
        else apply(cp)
        // keep going
        fixpoint
      case None =>
        // set the current control point
        curCp = None
        // finalize REPL
        if (useRepl) Repl.finished

    /** transfer function for control points */
    def apply(cp: ControlPoint): Unit = cp match
      case (np: NodePoint[_]) => this(np)
      case (rp: ReturnPoint)  => this(rp)

    /** transfer function for node points */
    def apply(np: NodePoint[_]): Unit

    /** transfer function for return points */
    def apply(rp: ReturnPoint): Unit

    /** transfer function for normal instructions */
    def transfer(
      inst: NormalInst,
    )(using np: NodePoint[_]): Updater

    /** transfer function for expressions */
    def transfer(
      expr: Expr,
    )(using np: NodePoint[Node]): Result[AbsValue]

    /** transfer function for unary operators */
    def transfer(
      unary: EUnary,
      operand: AbsValue,
    )(using np: NodePoint[Node]): AbsValue

    /** transfer function for binary operators */
    def transfer(
      binary: EBinary,
      left: AbsValue,
      right: AbsValue,
    )(using np: NodePoint[Node]): AbsValue

    /** transfer for variadic operators */
    def transfer(
      vop: VOp,
      vs: List[AbsValue],
    )(using np: NodePoint[Node]): AbsValue

    /** transfer for mathematical operators */
    def transfer(
      mop: MOp,
      vs: List[AbsValue],
    )(using np: NodePoint[Node]): AbsValue
  }
}
