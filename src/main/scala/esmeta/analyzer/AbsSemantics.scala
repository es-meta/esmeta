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
import scala.collection.mutable.{Map => MMap}

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

  /** a worklist of control points */
  val worklist: Worklist[ControlPoint] = QueueWorklist(npMap.keySet)

  /** the number of iterations */
  var iter: Int = 0

  /** count for each control point */
  var counter: Map[ControlPoint, Int] = Map()
  def getCount(cp: ControlPoint): Int = counter.getOrElse(cp, 0)

  /** RunJobs function */
  val runJobs = cfg.fnameMap("RunJobs")

  /** get return point of RunJobs */
  val runJobsRp = ReturnPoint(runJobs, View())

  /** get abstract return values and states of RunJobs */
  def finalResult: AbsRet = this(runJobsRp)

  /** set start time of analyzer */
  val startTime: Long = System.currentTimeMillis

  /** get elapsed time of analyzer */
  def elapsedTime: Long = System.currentTimeMillis - startTime

  /** set of analyzed functions */
  def analyzedFuncs: Set[Func] =
    npMap.keySet.map(_.func) ++ rpMap.keySet.map(_.func)

  /** set of analyzed nodes */
  def analyzedNodes: Set[Node] = npMap.keySet.map(_.node)

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
    if (!oldSt.isBottom && USE_REPL) REPL.merged = true
    if (!newSt.isBottom && !(newSt ⊑ oldSt))
      npMap += np -> (oldSt ⊔ newSt)
      worklist += np

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

  /** get string for result of all control points */
  def resultStrings: List[String] = resultStrings(None, false)
  def resultStrings(
    color: Option[String] = None,
    detail: Boolean = false,
  ): List[String] =
    npMap.keys.toList.sortBy(_.node.id).map(resultString(_, color, detail)) ++
    rpMap.keys.toList.sortBy(_.func.id).map(resultString(_, color, detail))

  /** get string for result of control points */
  def resultString(cp: ControlPoint, color: String, detail: Boolean): String =
    resultString(cp, Some(color), detail)

  /** get string for result of control points */
  def resultString(
    cp: ControlPoint,
    color: Option[String] = None,
    detail: Boolean = false,
  ): String =
    val func = cp.func.name
    val cpStr = cp.toString(detail = detail)
    val k = color.fold(cpStr)(setColor(_)(cpStr))
    cp match
      case np: NodePoint[_] =>
        val st = this(np).getString(detail = detail)
        s"$k -> $st"
      case rp: ReturnPoint =>
        val st = this(rp).getString(detail = detail)
        s"$k -> $st"

  /** check reachability */
  def reachable(np: NodePoint[Node]): Boolean = !apply(np).isBottom
  def reachable(rp: ReturnPoint): Boolean = !apply(rp).isBottom

  /** get detected type errors */
  def errors: Set[TypeError] =
    errorMap.map((_, innerMap) => innerMap.values.reduce(_ + _)).toSet

  // record type errors
  def +=(error: TypeError): Unit =
    val sensPoint = error.point
    val insensPoint = sensPoint.withoutView
    errorMap
      .getOrElseUpdate(insensPoint, MMap.empty)
      .update(sensPoint, error)
  private val errorMap = MMap[AnalysisPoint, MMap[AnalysisPoint, TypeError]]()

  /** conversion to string */
  override def toString: String = shortString

  /** conversion to short string */
  def shortString: String =
    s"- ${analyzedFuncs.size} functions are analyzed in $iter iterations."
}
