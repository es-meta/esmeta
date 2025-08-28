package esmeta.analyzer.eoggen

import esmeta.{ANALYZE_LOG_DIR, LINE_SEP}
import esmeta.analyzer.*
import esmeta.analyzer.eoggen.util.*
import esmeta.cfg.*
import esmeta.es.*
import esmeta.ir.{Func => _, *, given}
import esmeta.state.*
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import esmeta.util.HtmlUtils.escapeES

/** Execution Order Graph (EOG) generator */
class EOGGenerator(
  val cfg: CFG,
  val ast: Ast,
  val log: Boolean = false,
  override val useRepl: Boolean = false,
) extends Analyzer
  with AbsValueDecl
  with AbsStateDecl
  with AbsRetDecl
  with AbsTransferDecl
  with ViewDecl
  with EOGGenUtil {

  val irStringifier = IRElem.getStringifier(false, false)
  import irStringifier.given

  val esStringifier = ESElem.getStringifier(true, false, None)
  import esStringifier.given

  val stateStringifier = StateElem.getStringifier(true, false)
  import stateStringifier.given

  // initialize the analysis
  val initialNp = ast
    .getSdo("Evaluation")
    .fold(raise(s"no initial target found")) {
      case (ast0, func) =>
        val np = NodePoint(func, func.entry, emptyView)
        val st = AbsState(NAME_THIS -> AbsValue(ast0))
        npMap += np -> st
        np
    }

  // ---------------------------------------------------------------------------
  // Implementation for General Analyzer
  // ---------------------------------------------------------------------------
  /** worklist of control points */
  val worklist: Worklist[ControlPoint] = PriorityQueueWorklist(npMap.keySet)

  /** abstract transfer function */
  val transfer: AbsTransfer = new AbsTransfer

  /** check reachability of node points */
  def reachable(np: NodePoint[Node]): Boolean = npMap.keySet.contains(np)

  /** check reachability of return points */
  def reachable(rp: ReturnPoint): Boolean = rpMap.keySet.contains(rp)

  /** get string for result of control points */
  def getString(
    cp: ControlPoint,
    color: Option[String] = None,
    detail: Boolean = false,
  ): String =
    val func = cp.func.name
    val cpStr = cp.toString(detail = detail)
    val k = color.fold(cpStr)(setColor(_)(cpStr))
    cp match
      case np: NodePoint[_] =>
        val st = getResult(np)
        s"$k -> $st"
      case rp: ReturnPoint =>
        val ret = getResult(rp)
        s"$k -> $ret" + (
          if (detail)
            retEdges
              .getOrElse(rp, Set())
              .toList
              .sorted
              .map("\n  " + _.toString)
              .mkString(" -> [", ",", "\n]")
          else ""
        )

  /** update internal map */
  def +=(pair: (NodePoint[Node], AbsState)): Unit =
    val (np, newSt) = pair
    val oldSt = getResult(np)
    if (!oldSt.isBottom && useRepl) Repl.merged = true
    if (!(newSt ⊑ oldSt))
      npMap += np -> (oldSt ⊔ newSt)
      worklist += np

  /** logging the current analysis result */
  def logging: Unit = {}

  // ---------------------------------------------------------------------------
  // Implementation for EOG Generator
  // ---------------------------------------------------------------------------

  /** cache to get syntax-directed operation (SDO) */
  val getSdo = cached[(Ast, String), ChainResult](_.getSdo(_))

  /** hole AST for each sdo call with holes */
  var holeSdoInfo: Map[NodePoint[Call], Hole] = Map()

  // NOTE exit point should exists globally on EOGGenerator, not EOG
  val exitPoint: ControlPoint = ReturnPoint(initialNp.func, initialNp.view)
}
