package esmeta.analyzer.es

import esmeta.analyzer.*
import esmeta.cfg.*
import esmeta.es.*
import esmeta.ir.*
import esmeta.util.*

/** meta-level static analyzer for ECMAScript */
class ESAnalyzer(
  val cfg: CFG,
  val log: Boolean = false,
  override val useRepl: Boolean = false,
) extends Analyzer
  with AbsStateDecl
  with AbsRetDecl
  with AbsValueDecl
  with AbsTransferDecl
  with ViewDecl {

  case class Result(
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
  def analyze(sourceText: String): Result = {
    init(sourceText)
    transfer.fixpoint
    Result(npMap, rpMap, callInfo, retEdges, iter, counter)
  }

  // ---------------------------------------------------------------------------
  // Implementation for General Analyzer
  // ---------------------------------------------------------------------------
  /** worklist of control points */
  val worklist: Worklist[ControlPoint] = PriorityQueueWorklist(npMap.keySet)

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
  ): String = ???

  /** logging the current analysis result */
  def logging: Unit = ???

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------

  /** initialize analysis */
  private def init(sourceText: String): Unit = {
    val runJobs = cfg.fnameMap("RunJobs")
    val entry = runJobs.entry
    val initNp = NodePoint(runJobs, entry, View())
    val initSt = AbsState.Empty.define(
      Global(builtin.SOURCE_TEXT),
      AbsValue(sourceText),
    )

    // initialize analysis result
    npMap = Map(initNp -> initSt)
  }
}
object ESAnalyzer {

  /** configuration for ECMAScript analyzer */
  case class Config()
}
