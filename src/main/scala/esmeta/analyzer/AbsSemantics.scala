package esmeta.analyzer

import esmeta.analyzer.domain.*
import esmeta.analyzer.repl.*
import esmeta.cfg.*
import esmeta.ir.{Return, Func => IRFunc, Name, Param, Local}
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
  /** current control point */
  var curCp: Option[ControlPoint] = None,
) {

  /** analysis REPL */
  val repl = REPL(this)

  /** a worklist of control points */
  val worklist: Worklist[ControlPoint] = QueueWorklist(npMap.keySet)

  /** the number of iterations */
  def getIter: Int = iter
  private var iter: Int = 0

  /** count for each control point */
  def getCounter: Map[ControlPoint, Int] = counter
  def getCount(cp: ControlPoint): Int = counter.getOrElse(cp, 0)
  private var counter: Map[ControlPoint, Int] = Map()

  /** RunJobs function */
  val runJobs = cfg.fnameMap("RunJobs")

  /** get return point of RunJobs */
  val runJobsRp = ReturnPoint(runJobs, View())

  /** get abstract return values and states of RunJobs */
  def finalResult: AbsRet = this(runJobsRp)

  /** abstract transfer function */
  val transfer: AbsTransfer = AbsTransfer(this)

  /** set start time of analyzer */
  val startTime: Long = System.currentTimeMillis

  /** set of analyzed functions */
  def analyzedFuncs: Set[Func] =
    npMap.keySet.map(_.func) ++ rpMap.keySet.map(_.func)

  /** fixpiont computation */
  @tailrec
  final def fixpoint: this.type = worklist.next match
    case Some(cp) =>
      // set the current control point
      curCp = Some(cp)
      // count how many visited for each control point
      counter += cp -> (getCount(cp) + 1)
      // increase iteration number
      iter += 1
      // check time limit
      if (iter % CHECK_PERIOD == 0) TIME_LIMIT.map(limit => {
        val duration = (System.currentTimeMillis - startTime) / 1000
        if (duration > limit) exploded("timeout")
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
      // set the current control point
      curCp = None
      // finalize REPL
      if (USE_REPL) repl.finished
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
    method: Boolean = false,
  ): Unit =
    this.callInfo += callerNp -> callerSt
    for {
      (calleeNp, calleeSt) <- getCalleeEntries(
        callerNp,
        callerSt,
        calleeFunc,
        args,
        captured,
        method,
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

  /** call transition */
  def getCalleeEntries(
    callerNp: NodePoint[Call],
    callerSt: AbsState,
    calleeFunc: Func,
    args: List[AbsValue],
    captured: Map[Name, AbsValue],
    method: Boolean,
  ): List[(NodePoint[_], AbsState)] = {
    // handle ir callsite sensitivity
    val NodePoint(callerFunc, callSite, callerView) = callerNp
    val baseView =
      if (IR_SENS)
        callerView.copy(
          calls = callSite :: callerView.calls,
          intraLoopDepth = 0,
        )
      else callerView

    val calleeNp = NodePoint(calleeFunc, calleeFunc.entry, baseView)
    val calleeRp = ReturnPoint(calleeFunc, baseView)
    val calleeSt = callerSt.copied(locals =
      getLocals(
        callerNp,
        calleeRp,
        args,
        cont = false,
        method,
      ) ++ captured,
    )
    List((calleeNp, calleeSt))
  }

  /** get local variables */
  def getLocals(
    callerNp: NodePoint[Call],
    calleeRp: ReturnPoint,
    args: List[AbsValue],
    cont: Boolean,
    method: Boolean,
  ): Map[Local, AbsValue] = {
    val params: List[Param] = calleeRp.func.irFunc.params
    var map = Map[Local, AbsValue]()

    @tailrec
    def aux(ps: List[Param], as: List[AbsValue]): Unit = (ps, as) match {
      case (Nil, Nil) =>
      case (Param(lhs, _, optional, _) :: pl, Nil) =>
        if (optional) {
          map += lhs -> AbsValue(Absent)
          aux(pl, Nil)
        }
      case (Nil, args) =>
      // XXX Handle GeneratorStart <-> GeneratorResume arith mismatch
      case (param :: pl, arg :: al) =>
        map += param.lhs -> arg
        aux(pl, al)
    }
    aux(params, args)
    map
  }

  /** update return points */
  def doReturn(elem: Return, rp: ReturnPoint, origRet: AbsRet): Unit =
    val ReturnPoint(func, view) = rp
    val retRp = ReturnPoint(func, getEntryView(view))
    // wrap completion by conditions specified in
    // [5.2.3.5 Implicit Normal Completion]
    // (https://tc39.es/ecma262/#sec-implicit-normal-completion)
    val newRet = if (func.isReturnComp) origRet.wrapCompletion else origRet
    if (!newRet.value.isBottom)
      val oldRet = this(retRp)
      if (!oldRet.isBottom && USE_REPL) repl.merged = true
      if (newRet !⊑ oldRet)
        rpMap += retRp -> (oldRet ⊔ newRet)
        worklist += retRp

  /** loop transition for next views */
  def loopNext(view: View): View = view.loops match
    case LoopCtxt(loop, k) :: rest if IR_SENS =>
      view.copy(loops = LoopCtxt(loop, k + 1) :: rest)
    case _ => view

  /** loop transition for function enter */
  def loopEnter(view: View, loop: Branch): View =
    val loopView =
      if (IR_SENS)
        view.copy(
          loops = LoopCtxt(loop, 0) :: view.loops,
          intraLoopDepth = view.intraLoopDepth + 1,
        )
      else view
    loopOut += loopView -> (loopOut.getOrElse(loopView, Set()) + view)
    loopView

  /** loop transition for bases */
  def loopBase(view: View): View = view.loops match
    case LoopCtxt(loop, k) :: rest if IR_SENS =>
      view.copy(loops = LoopCtxt(loop, 0) :: rest)
    case _ => view

  /** loop transition for function exits */
  def loopExit(view: View): View = if (IR_SENS) {
    val views = loopOut.getOrElse(loopBase(view), Set())
    views.size match
      case 0 => error("invalid loop exit")
      case 1 => views.head
      case _ => exploded("loop is too merged.")
  } else view

  /** get entry views of loops */
  @tailrec
  final def getEntryView(view: View): View =
    if (!IR_SENS | view.intraLoopDepth == 0) view
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
    val k = setColor(color)(cpStr)
    cp match
      case np: NodePoint[_] =>
        val st = this(np).getString(detail = detail)
        s"""$k -> $st
           |${np.node}""".stripMargin
      case rp: ReturnPoint =>
        val st = this(rp).getString(detail = detail)
        s"""$k -> $st"""

  /** check reachability */
  def reachable(np: NodePoint[Node]): Boolean = !apply(np).isBottom
  def reachable(rp: ReturnPoint): Boolean = !apply(rp).isBottom

  /** conversion to string */
  override def toString: String = shortString

  /** conversion to short string */
  def shortString: String =
    s"- ${analyzedFuncs.size} functions are analyzed in $iter iterations."
}
