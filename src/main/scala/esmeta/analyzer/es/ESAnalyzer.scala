package esmeta.analyzer.es

import esmeta.analyzer.*
import esmeta.cfg.*
import esmeta.es.*
import esmeta.ir.*
import esmeta.state.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.domain.*

/** meta-level static analyzer for ECMAScript */
class ESAnalyzer(
  val cfg: CFG,
  val log: Boolean = false,
  val irSens: Boolean = true,
  override val detail: Boolean = false,
  override val location: Boolean = false,
  override val useRepl: Boolean = false,
) extends Analyzer
  with AbsStateDecl
  with AbsRetDecl
  with AbsValueDecl
  with AbsHeapDecl
  with AbsObjDecl
  with BindingDecl
  with AbsPrimValueDecl
  with AbsAddrDecl
  with AbsCloDecl
  with AbsContDecl
  with AbsRefTargetDecl
  with AbsTransferDecl
  with ViewDecl {

  /** loop out edges */
  protected var loopOut: Map[View, Set[View]] = Map()

  /** analysis result */
  case class AnalysisResult(
    npMap: Map[NodePoint[Node], AbsState],
    rpMap: Map[ReturnPoint, AbsRet],
    callInfo: Map[NodePoint[Call], AbsState],
    retEdges: Map[ReturnPoint, Set[NodePoint[Call]]],
    iter: Int,
    counter: Map[ControlPoint, Int],
  ) {
    override def toString: String = ???
  }

  /** perform type analysis with the given control flow graph */
  def analyze(sourceText: String): AnalysisResult = {
    init(sourceText)
    transfer.fixpoint
    AnalysisResult(npMap, rpMap, callInfo, retEdges, iter, counter)
  }

  // ---------------------------------------------------------------------------
  // Implementation for General Analyzer
  // ---------------------------------------------------------------------------
  /** worklist of control points */
  val worklist: Worklist[ControlPoint] = new PriorityQueueWorklist

  /** abstract transfer function */
  val transfer: AbsTransfer = new AbsTransfer

  /** check reachability of node points */
  def reachable(np: NodePoint[Node]): Boolean = !getResult(np).isBottom

  /** check reachability of return points */
  def reachable(rp: ReturnPoint): Boolean = !getResult(rp).isBottom

  /** get string for result of control points */
  def getString(
    cp: ControlPoint,
    color: Option[String],
    detail: Boolean,
  ): String = {
    val func = cp.func.name
    val cpStr = cp.toString
    val k = color.fold(cpStr)(setColor(_)(cpStr))
    cp match
      case np: NodePoint[_] => s"$k -> ${getResult(np)}"
      case rp: ReturnPoint  => s"$k -> ${getResult(rp)}"
  }

  /** logging the current analysis result */
  def logging: Unit = ???

  // ---------------------------------------------------------------------------
  // helper functions
  // ---------------------------------------------------------------------------
  /** update internal map */
  protected def +=(pair: (NodePoint[Node], AbsState)): Unit =
    val (np, newSt) = pair
    val oldSt = getResult(np)
    if (!oldSt.isBottom && useRepl) Repl.merged = true
    if (!newSt.isBottom && !(newSt ⊑ oldSt))
      npMap += np -> (oldSt ⊔ newSt)
      worklist += np

  /** initialize analysis */
  protected def init(sourceText: String): Unit = {
    val runJobs = cfg.fnameMap("RunJobs")
    val entry = runJobs.entry
    val initNp = NodePoint(runJobs, entry, View())
    val initSt = AbsState.Empty.define(
      Global(builtin.SOURCE_TEXT),
      AbsValue(sourceText),
    )

    // initialize analysis result
    npMap = Map(initNp -> initSt)

    // initialize worklist
    worklist.reset(npMap.keySet)
  }
}
object ESAnalyzer {

  /** configuration for ECMAScript analyzer */
  case class Config()
}
