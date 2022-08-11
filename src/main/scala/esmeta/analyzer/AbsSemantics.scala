package esmeta.analyzer

import esmeta.DEBUG
import esmeta.analyzer.domain.*
import esmeta.analyzer.util.*
import esmeta.cfg.*
import esmeta.error.AnalysisImprecise
import esmeta.ir.{Name, Local}
import esmeta.js.Ast
import esmeta.util.*
import esmeta.util.BaseUtils.*
import scala.Console.*
import scala.annotation.tailrec

/** abstract semantics */
case class AbsSemantics(
  var npMap: Map[NodePoint[Node], AbsState] = Map(),
  var rpMap: Map[ReturnPoint, AbsRet] = Map(),
  var callInfo: Map[NodePoint[Call], AbsState] = Map(),
  var retEdges: Map[ReturnPoint, Set[NodePoint[Call]]] = Map(),
  var loopOut: Map[View, Set[View]] = Map(),
  timeLimit: Option[Long] = None,
) {

  /** assertions */
  var assertions: Map[ControlPoint, AbsValue] = Map()
  def checkAssertion: Unit = for { (cp, v) <- assertions } {
    if (AVF == v) warning("assertion failed", cp)
  }

  /** repl */
  val repl = REPL(this)

  /** a worklist of control points */
  val worklist: Worklist[ControlPoint] = new QueueWorklist(npMap.keySet)

  /** the number of iterations */
  def getIter: Int = iter
  private var iter: Int = 0

  /** get abstract return values and states of RunJobs */
  val runJobs = cfg.fnameMap("RunJobs")
  val runJobsRp = ReturnPoint(runJobs, View())
  def finalResult: AbsRet = this(runJobsRp)

  /** abstract transfer function */
  val transfer: AbsTransfer = AbsTransfer(this)

  /** set start time of analyzer */
  val startTime: Long = System.currentTimeMillis

  /** iteration period for check */
  val CHECK_PERIOD = 10000

  /** fixpiont computation */
  @tailrec
  final def fixpoint: AbsSemantics = worklist.next match {
    case Some(cp) => {
      iter += 1

      // check time limit
      if (iter % CHECK_PERIOD == 0) timeLimit.map(limit => {
        val duration = (System.currentTimeMillis - startTime) / 1000
        if (duration > limit) throw AnalysisImprecise("timeout")
      })

      // text-based debugging
      if (DEBUG) println(s"${cp.func.name}:$cp")

      // run REPL
      if (USE_REPL) repl(transfer, cp)

      // abstract transfer for the current control point
      else transfer(cp)

      // check soundness using concrete execution
      // checkWithInterp.map(_.runAndCheck)

      // keep going
      fixpoint
    }
    case None =>
      // finalize REPL
      if (USE_REPL) repl.finished

      // checker
      checkAssertion

      // final result
      this
  }

  /** get return edges */
  def getRetEdges(rp: ReturnPoint): Set[NodePoint[Call]] =
    retEdges.getOrElse(rp, Set())

  /** lookup */
  def apply(np: NodePoint[Node]): AbsState = npMap.getOrElse(np, AbsState.Bot)
  def apply(rp: ReturnPoint): AbsRet = rpMap.getOrElse(rp, AbsRet.Bot)

  /** update internal map */
  def +=(pair: (NodePoint[Node], AbsState)): Unit = {
    val (np, newSt) = pair
    val oldSt = this(np)
    if (!oldSt.isBottom && USE_REPL) repl.merged = true
    if (!newSt.isBottom && !(newSt ⊑ oldSt)) {
      npMap += np -> (oldSt ⊔ newSt)
      worklist += np
    }
  }

  /** handle calls */
  def doCall(
    callerNp: NodePoint[Call],
    callerSt: AbsState,
    calleeFunc: Func,
    args: List[AbsValue],
    captured: Map[Name, AbsValue] = Map(),
  ): Unit = {
    this.callInfo += callerNp -> callerSt

    getCalleeEntries(callerNp, callerSt, calleeFunc, args, captured).foreach {
      case (calleeNp, calleeSt) =>
        // add callee to worklist
        this += calleeNp -> calleeSt.doCall

        // add return edges from callee to caller
        val rp = ReturnPoint(calleeFunc, calleeNp.view)
        val set = retEdges.getOrElse(rp, Set())
        retEdges += rp -> (set + callerNp)

        // propagate callee analysis result
        val retT = this(rp)
        if (!retT.isBottom) worklist += rp
    }
  }

  /** handle sensitivity */
  def handleSens[T](l: List[T], bound: Int): List[T] =
    if (IR_SENS) if (INF_SENS) l else l.take(bound)
    else Nil
  def handleSens(n: Int, bound: Int): Int =
    if (IR_SENS) if (INF_SENS) n else n min bound
    else 0

  /** call transition */
  def getCalleeEntries(
    callerNp: NodePoint[Call],
    callerSt: AbsState,
    calleeFunc: Func,
    args: List[AbsValue],
    captured: Map[Name, AbsValue],
  ): List[(NodePoint[_], AbsState)] = {
    // handle ir callsite sensitivity
    val NodePoint(callerFunc, callSite, callerView) = callerNp
    val baseView = callerView.copy(
      calls = handleSens(callSite :: callerView.calls, IR_CALL_DEPTH),
      intraLoopDepth = 0,
    )

    // get typed arguments
    val typedArgsList =
      args.map(_.getTypedArguments).foldRight(List(List[(AbsValue, Type)]())) {
        case (typedArgList, argsList) =>
          for {
            curr <- argsList
            (v, ty) <- typedArgList
          } yield (v, ty) :: curr
      }

    // get callee entry state for each arguments list
    for {
      typedArgs <- typedArgsList
      (currArgs, currTys) = typedArgs.unzip
      calleeSt = callerSt.copied(locals =
        getLocals(calleeFunc, currArgs) ++ captured,
      )
      // handle type sensitivity
      calleeView =
        if (TYPE_SENS) baseView.copy(tys = currTys)
        else baseView
      calleeNp = NodePoint(calleeFunc, calleeFunc.entry.get, calleeView)
    } yield (calleeNp, calleeSt)
  }

  /** update return points */
  def doReturn(rp: ReturnPoint, newRet: AbsRet): Unit = {
    val ReturnPoint(func, view) = rp
    val retRp = ReturnPoint(func, getEntryView(view))
    if (!newRet.value.isBottom) {
      val oldRet = this(retRp)
      if (!oldRet.isBottom && USE_REPL) repl.merged = true
      if (newRet !⊑ oldRet) {
        rpMap += retRp -> (oldRet ⊔ newRet)
        worklist += retRp
      }
    }
  }

  /** loop transition */
  def loopNext(view: View): View = view.loops match
    case LoopCtxt(loop, k) :: rest =>
      view.copy(loops = LoopCtxt(loop, handleSens(k + 1, LOOP_ITER)) :: rest)
    case _ => view
  def loopEnter(view: View, loop: Branch): View =
    val loopView = view.copy(
      loops = handleSens(LoopCtxt(loop, 0) :: view.loops, LOOP_DEPTH),
      intraLoopDepth = view.intraLoopDepth + 1,
    )
    loopOut += loopView -> (loopOut.getOrElse(loopView, Set()) + view)
    loopView
  def loopBase(view: View): View = view.loops match
    case LoopCtxt(loop, k) :: rest =>
      view.copy(loops = LoopCtxt(loop, 0) :: rest)
    case _ => view
  def loopExit(view: View): View =
    val views = loopOut.getOrElse(loopBase(view), Set())
    views.size match
      case 0 => ???
      case 1 => views.head
      case _ => exploded("loop is too merged.")

  /** get entry views of loops */
  @tailrec
  final def getEntryView(view: View): View =
    if (view.intraLoopDepth == 0) view
    else getEntryView(loopExit(view))

  /** get abstract state of control points */
  def getState(cp: ControlPoint): AbsState = cp match
    case np: NodePoint[_] => this(np)
    case rp: ReturnPoint  => this(rp).state

  /** get string for result of control points */
  def getString(
    cp: ControlPoint,
    color: String,
    detail: Boolean,
  ): String = {
    val func = cp.func.name
    val cpStr = cp.toString(detail = detail)
    val k = setColor(color)(s"$func:$cpStr")
    val v = cp match {
      case (np: NodePoint[_]) => this(np).toString(detail = detail)
      case (rp: ReturnPoint)  => this(rp).toString(detail = detail)
    }
    s"$k -> $v"
  }

  /** check reachability based on call contexts */
  def reachable(np: NodePoint[Node]): Boolean =
    !getNps(np).forall(this(_).isBottom)
  def getNps(givenNp: NodePoint[Node]): Set[NodePoint[Node]] = {
    val entryView = getEntryView(givenNp.view)
    for {
      np <- npMap.keySet
      if givenNp.node == np.node && entryView == getEntryView(np.view)
    } yield np
  }

  /** string */
  override def toString: String = {
    val funcs = npMap.keySet.map(_.func) ++ rpMap.keySet.map(_.func)
    s"${funcs.size} functions are analyzed in $iter iterations."
  }
}
object AbsSemantics {

  /** constructors */
  def apply(sourceText: String): AbsSemantics =
    AbsSemantics(npMap = Initialize.initJs(sourceText))
  def apply(initNpMap: Map[NodePoint[Node], AbsState]): AbsSemantics =
    AbsSemantics(npMap = initNpMap)
}
