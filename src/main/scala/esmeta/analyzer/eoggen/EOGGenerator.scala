package esmeta.analyzer.eoggen

import esmeta.{ANALYZE_LOG_DIR, LINE_SEP}
import esmeta.analyzer.*
import esmeta.cfg.*
import esmeta.es.*
import esmeta.ir.{Func => _, *, given}
import esmeta.state.*
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*

/** Execution Order Graph (EOG) generator */
class EOGGenerator(
  val cfg: CFG,
  val ast: Ast,
  val log: Boolean = false,
) extends Analyzer
  with AbsValueDecl
  with AbsStateDecl
  with AbsRetDecl
  with AbsTransferDecl
  with ViewDecl {

  val irStringifier = IRElem.getStringifier(false, false)
  import irStringifier.given

  val esStringifier = ESElem.getStringifier(true, false, None)
  import esStringifier.given

  val stateStringifier = StateElem.getStringifier(true, false)
  import stateStringifier.given

  // initialize the analysis
  ast.getSdo("Evaluation").map { (ast0, func) =>
    val np = NodePoint(func, func.entry, emptyView)
    val st = AbsState(NAME_THIS -> AbsValue(ast0))
    npMap += np -> st
  }

  // ---------------------------------------------------------------------------
  // Implementation for General Analyzer
  // ---------------------------------------------------------------------------
  /** worklist of control points */
  val worklist: Worklist[ControlPoint] = PriorityQueueWorklist(npMap.keySet)

  /** abstract transfer function */
  val transfer: AbsTransfer = new AbsTransfer

  /** check reachability of node points */
  def reachable(np: NodePoint[Node]): Boolean = ???

  /** check reachability of return points */
  def reachable(rp: ReturnPoint): Boolean = ???

  /** get string for result of control points */
  def getString(
    cp: ControlPoint,
    color: Option[String] = None,
    detail: Boolean = false,
  ): String = ???

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
  private val getSdo = cached[(Ast, String), Option[(Ast, Func)]](_.getSdo(_))
}
