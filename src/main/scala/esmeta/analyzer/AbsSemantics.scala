package esmeta.analyzer

import esmeta.DEBUG
import esmeta.analyzer.domain.*
import esmeta.analyzer.util.*
import esmeta.cfg.*
import esmeta.error.AnalysisImprecise
import esmeta.js.Ast
import esmeta.util.*
import esmeta.util.BaseUtils.*
import scala.Console.*
import scala.annotation.tailrec

/** abstract semantics */
case class AbsSemantics(
  val sourceText: String,
  val cachedAst: Option[Ast] = None,
  var npMap: Map[NodePoint[Node], AbsState] = Map(),
  var rpMap: Map[ReturnPoint, AbsRet.Elem] = Map(),
  var callInfo: Map[NodePoint[Call], AbsState] = Map(),
  var retEdges: Map[ReturnPoint, Set[NodePoint[Call]]] = Map(),
  var loopOut: Map[View, Set[View]] = Map(),
  timeLimit: Option[Long] = Some(ANALYZE_TIMEOUT),
  execLevel: Int = 0,
) {
  // repl
  val repl = REPL(this)

  // a worklist of control points
  val worklist: Worklist[ControlPoint] = new QueueWorklist(npMap.keySet)

  // the number of iterations
  def getIter: Int = iter
  private var iter: Int = 0

  // concrete execution
  val checkWithInterp: Option[CheckWithInterp] =
    if (execLevel >= 1)
      Some(CheckWithInterp(this, sourceText, cachedAst, execLevel))
    else None

  // get abstract return values and states of RunJobs
  val runJobs = cfg.fnameMap("RunJobs")
  val runJobsRp = ReturnPoint(runJobs, View())
  def finalResult: AbsRet = this(runJobsRp)

  // abstract transfer function
  val transfer: AbsTransfer = AbsTransfer(this)

  // set start time of analyzer
  val startTime: Long = System.currentTimeMillis

  // iteration period for check
  val CHECK_PERIOD = 10000

  // fixpiont computation
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
      checkWithInterp.map(_.runAndCheck)

      // keep going
      fixpoint
    }
    case None =>
      // finalize REPL
      if (USE_REPL) repl.finished

      // final result
      this
  }

  // get return edges
  def getRetEdges(rp: ReturnPoint): Set[NodePoint[Call]] =
    retEdges.getOrElse(rp, Set())

  // lookup
  def apply(np: NodePoint[Node]): AbsState = npMap.getOrElse(np, AbsState.Bot)
  def apply(rp: ReturnPoint): AbsRet = rpMap.getOrElse(rp, AbsRet.Bot)

  // update internal map
  def +=(pair: (NodePoint[Node], AbsState)): Unit = {
    val (np, newSt) = pair
    val oldSt = this(np)
    if (!oldSt.isBottom && USE_REPL) repl.merged = true
    if (!newSt.isBottom && !(newSt ⊑ oldSt)) {
      npMap += np -> (oldSt ⊔ newSt)
      worklist += np
    }
  }

  // handle calls
  def doCall(
    call: Call,
    callerView: View,
    callerSt: AbsState,
    func: Func,
    st: AbsState,
    astOpt: Option[Ast] = None,
  ): Unit = {
    val callerNp = NodePoint(cfg.funcOf(call), call, callerView)
    this.callInfo += callerNp -> callerSt

    val calleeView = viewCall(callerView, call)
    val np = NodePoint(func, func.entry.get, calleeView)
    this += np -> st.doCall

    val rp = ReturnPoint(func, calleeView)
    val set = retEdges.getOrElse(rp, Set())
    retEdges += rp -> (set + callerNp)

    val retT = this(rp)
    if (!retT.isBottom) worklist += rp
  }

  // handle sensiticity
  def handleSens[T](l: List[T], bound: Int): List[T] =
    if (INF_SENS) l else l.take(bound)
  def handleSens(n: Int, bound: Int): Int =
    if (INF_SENS) n else n min bound

  // call transition
  def viewCall(
    callerView: View,
    call: Call,
  ): View = {
    val View(calls, _, _) = callerView
    val view = callerView.copy(
      calls = handleSens(call :: calls, IR_CALL_DEPTH),
      intraLoopDepth = 0,
    )
    view
  }

  // update return points
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

  // loop transition
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

  // get entry views of loops
  @tailrec
  final def getEntryView(view: View): View = {
    if (view.intraLoopDepth == 0) view
    else getEntryView(loopExit(view))
  }

  // get abstract state of control points
  def getState(cp: ControlPoint): AbsState = cp match
    case np: NodePoint[_] => this(np)
    case rp: ReturnPoint  => this(rp).state

  // get string for result of control points
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

  // check reachability based on call contexts
  def reachable(np: NodePoint[Node]): Boolean =
    !getNps(np).forall(this(_).isBottom)
  def getNps(givenNp: NodePoint[Node]): Set[NodePoint[Node]] = {
    val entryView = getEntryView(givenNp.view)
    for {
      np <- npMap.keySet
      if givenNp.node == np.node && entryView == getEntryView(np.view)
    } yield np
  }
}
object AbsSemantics {
  // constructors
  def apply(
    sourceText: String,
    cachedAst: Option[Ast],
    execLevel: Int,
  ): AbsSemantics = AbsSemantics(
    npMap = Initialize(sourceText),
    sourceText = sourceText,
    cachedAst = cachedAst,
    execLevel = execLevel,
  )
}
