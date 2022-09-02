package esmeta.analyzer

import esmeta.analyzer.Config.*
import esmeta.analyzer.domain.*
import esmeta.analyzer.repl.*
import esmeta.cfg.*
import esmeta.ir.{Func => IRFunc, Name, Param, Local}
import esmeta.error.*
import esmeta.state.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import scala.Console.*
import scala.annotation.tailrec

/** abstract semantics */
class AbsSemantics(
  /** abstract states in each node point */
  var npMap: Map[NodePoint[Node], AbsState] = Map(),
  /** abstract states in each return point */
  var rpMap: Map[ReturnPoint, AbsRet] = Map(),
  /** abstract states right before calling functions */
  var callInfo: Map[NodePoint[Call], AbsState] = Map(),
  /** return edges */
  var retEdges: Map[ReturnPoint, Set[NodePoint[Call]]] = Map(),
  /** loop out edges */
  var loopOut: Map[View, Set[View]] = Map(),
) {

  /** assertions */
  var assertions: Map[ControlPoint, AbsValue] = Map()
  def checkAssertion: Unit = for ((cp, v) <- assertions)
    if (AVF == v) warning("assertion failed", cp)

  /** current control point */
  var curCP: Option[ControlPoint] = None

  /** show warning messages */
  def warning(msg: String): Unit = warning(msg, curCP)

  /** show warning messages with optinal control points */
  def warning(msg: String, cp: Option[ControlPoint]): Unit = cp match
    case Some(cp) => warning(msg, cp)
    case None     => printlnColor(RED)(msg)

  /** show warning messages with control points */
  def warning(msg: String, cp: ControlPoint): Unit =
    printlnColor(RED)(s"[$cp @ ${cp.func.name}]: $msg")

  /** analysis REPL */
  val repl = REPL(this)

  /** a worklist of control points */
  val worklist: Worklist[ControlPoint] = QueueWorklist(npMap.keySet)

  /** the number of iterations */
  def getIter: Int = iter
  private var iter: Int = 0

  /** RunJobs function */
  val runJobs = Config.cfg.fnameMap("RunJobs")

  /** get return point of RunJobs */
  val runJobsRp = ReturnPoint(runJobs, View())

  /** get abstract return values and states of RunJobs */
  def finalResult: AbsRet = this(runJobsRp)

  /** abstract transfer function */
  val transfer: AbsTransfer = AbsTransfer(this)

  /** set start time of analyzer */
  val startTime: Long = System.currentTimeMillis

  /** fixpiont computation */
  @tailrec
  final def fixpoint: AbsSemantics = worklist.next match
    case Some(cp) =>
      // increase iteration number
      iter += 1
      // check time limit
      if (iter % CHECK_PERIOD == 0) TIME_LIMIT.map(limit => {
        val duration = (System.currentTimeMillis - startTime) / 1000
        if (duration > limit) throw AnalysisImprecise("timeout")
      })
      // text-based debugging
      if (DEBUG) println(s"${cp.func.name}:$cp")
      // run REPL
      if (USE_REPL) repl(transfer, cp)
      // abstract transfer for the current control point
      else transfer(cp)
      // keep going
      fixpoint
    case None =>
      // finalize REPL
      if (USE_REPL) repl.finished
      // checker
      checkAssertion
      // final result
      this

  /** get return edges */
  def getRetEdges(rp: ReturnPoint): Set[NodePoint[Call]] =
    retEdges.getOrElse(rp, Set())

  /** lookup for node points */
  def apply(np: NodePoint[Node]): AbsState = npMap.getOrElse(np, AbsState.Bot)

  /** lookup for return points */
  def apply(rp: ReturnPoint): AbsRet = rpMap.getOrElse(rp, AbsRet.Bot)

  /** update internal map */
  def +=(pair: (NodePoint[Node], AbsState)): Unit =
    val (np, newSt) = pair
    val oldSt = this(np)
    if (!oldSt.isBottom && USE_REPL) repl.merged = true
    if (!newSt.isBottom && !(newSt ⊑ oldSt))
      npMap += np -> (oldSt ⊔ newSt)
      worklist += np

  /** handle calls */
  def doCall(
    callerNp: NodePoint[Call],
    callerSt: AbsState,
    calleeFunc: Func,
    args: List[AbsValue],
    captured: Map[Name, AbsValue] = Map(),
  ): Unit =
    this.callInfo += callerNp -> callerSt
    for {
      (calleeNp, calleeSt) <- getCalleeEntries(
        callerNp,
        callerSt,
        calleeFunc,
        args,
        captured,
      )
    } {
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

  /** TODO handle sensitivity */
  def handleSens[T](l: List[T], bound: Int): List[T] =
    if (IR_SENS) if (INF_SENS) l else l.take(bound)
    else Nil

  /** TODO handle sensitivity for depths */
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

    // TODO get typed arguments
    // val typedArgsList =
    //   args.map(_.getTypedArguments).foldRight(List(List[(AbsValue, Type)]())) {
    //     case (typedArgList, argsList) =>
    //       for {
    //         curr <- argsList
    //         (v, ty) <- typedArgList
    //       } yield (v, ty) :: curr
    //   }

    // TODO get callee entry state for each arguments list
    // for {
    //   // TODO typedArgs <- typedArgsList
    //   // TODO (currArgs, currTys) = typedArgs.unzip
    //   calleeSt = callerSt.copied(locals =
    //     getLocals(calleeFunc, currArgs) ++ captured,
    //   )
    //   // handle type sensitivity
    //   calleeView = baseView
    //   // TODO calleeView =
    //   //   if (TYPE_SENS) baseView.copy(tys = currTys)
    //   //   else baseView
    //   calleeNp = NodePoint(calleeFunc, calleeFunc.entry.get, calleeView)
    // } yield (calleeNp, calleeSt)

    val calleeSt = callerSt.copied(locals =
      getLocals(calleeFunc, args) ++ captured,
    )
    val calleeNp = NodePoint(calleeFunc, calleeFunc.entry.get, baseView)
    List((calleeNp, calleeSt))
  }

  /** get local variables */
  def getLocals(
    func: Func,
    args: List[AbsValue],
    cont: Boolean = false,
  ): Map[Local, AbsValue] = {
    val params = func.irFunc.params
    var map = Map[Local, AbsValue]()

    @tailrec
    def aux(ps: List[Param], as: List[AbsValue]): Unit = (ps, as) match {
      case (Nil, Nil) =>
      case (Param(lhs, _, optional) :: pl, Nil) =>
        if (optional) {
          map += lhs -> AbsValue(Absent)
          aux(pl, Nil)
        } else throw AnalysisRemainingParams(ps)
      case (Nil, args) =>
        // XXX Handle GeneratorStart <-> GeneratorResume arith mismatch
        if (!cont) error("") // TODO throw AnalysisRemainingArgs(args)
      case (param :: pl, arg :: al) =>
        map += param.lhs -> arg
        aux(pl, al)
    }
    aux(params, args)
    map
  }

  /** update return points */
  def doReturn(rp: ReturnPoint, newRet: AbsRet): Unit =
    val ReturnPoint(func, view) = rp
    val retRp = ReturnPoint(func, getEntryView(view))
    if (!newRet.value.isBottom)
      val oldRet = this(retRp)
      if (!oldRet.isBottom && USE_REPL) repl.merged = true
      if (newRet !⊑ oldRet)
        rpMap += retRp -> (oldRet ⊔ newRet)
        worklist += retRp

  /** loop transition for next views */
  def loopNext(view: View): View = view.loops match
    case LoopCtxt(loop, k) :: rest =>
      view.copy(loops = LoopCtxt(loop, handleSens(k + 1, LOOP_ITER)) :: rest)
    case _ => view

  /** loop transition for function enter */
  def loopEnter(view: View, loop: Branch): View =
    val loopView = view.copy(
      loops = handleSens(LoopCtxt(loop, 0) :: view.loops, LOOP_DEPTH),
      intraLoopDepth = view.intraLoopDepth + 1,
    )
    loopOut += loopView -> (loopOut.getOrElse(loopView, Set()) + view)
    loopView

  /** loop transition for bases */
  def loopBase(view: View): View = view.loops match
    case LoopCtxt(loop, k) :: rest =>
      view.copy(loops = LoopCtxt(loop, 0) :: rest)
    case _ => view

  /** loop transition for function exits */
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
  ): String =
    val func = cp.func.name
    val cpStr = cp.toString(detail = detail)
    val k = setColor(color)(s"$func:$cpStr")
    val v = cp match {
      case (np: NodePoint[_]) => this(np).getString(detail = detail)
      case (rp: ReturnPoint)  => this(rp).getString(detail = detail)
    }
    s"$k -> $v"

  /** check reachability based on call contexts */
  def reachable(np: NodePoint[Node]): Boolean =
    !getNps(np).forall(this(_).isBottom)

  /** get node points */
  def getNps(givenNp: NodePoint[Node]): Set[NodePoint[Node]] =
    val entryView = getEntryView(givenNp.view)
    for {
      np <- npMap.keySet
      if givenNp.node == np.node && entryView == getEntryView(np.view)
    } yield np

  /** string */
  override def toString: String =
    val funcs = npMap.keySet.map(_.func) ++ rpMap.keySet.map(_.func)
    s"${funcs.size} functions are analyzed in $iter iterations."
}
