package esmeta.analyzer.astflow

import esmeta.{ANALYZE_LOG_DIR, LINE_SEP}
import esmeta.analyzer.*
import esmeta.cfg.*
import esmeta.ir.{Func => _, *, given}
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*

/** ast flow analyzer in ECMA-262 */
class AstFlowAnalyzer(
  val cfg: CFG,
  val log: Boolean = false,
) extends Analyzer
  with AbsValueDecl
  with AbsStateDecl
  with AbsRetDecl
  with AbsTransferDecl
  with ViewDecl {

  val irStringifier = IRElem.getStringifier(false, false)
  import irStringifier.given

  // initialization
  npMap = getInitNpMap(cfg.funcs)

  // ---------------------------------------------------------------------------
  // Implementation for General Analyzer
  // ---------------------------------------------------------------------------
  /** worklist of control points */
  val worklist: Worklist[ControlPoint] = PriorityQueueWorklist(npMap.keySet)

  /** abstract transfer function */
  val transfer: AbsTransfer = new AbsTransfer

  /** check reachability of node points */
  def reachable(np: NodePoint[Node]): Boolean = true

  /** check reachability of return points */
  def reachable(rp: ReturnPoint): Boolean = true

  /** get initial abstract states in each node point */
  private def getInitNpMap(
    targets: List[Func],
  ): Map[NodePoint[Node], AbsState] = (for {
    func <- targets
    entry = func.entry
    np = NodePoint(func, entry, emptyView)
    st = getState(func)
  } yield np -> st).toMap

  /** get initial state of function */
  private def getState(func: Func): AbsState =
    func.params.foldLeft(AbsState.Bot) {
      case (st, param) =>
        val x = param.lhs
        val v = AbsValue.param(param)
        st.update(x, v)
    }

  /** update internal map */
  def +=(pair: (NodePoint[Node], AbsState)): Unit =
    val (np, newSt) = pair
    val oldSt = getResult(np)
    if (!oldSt.isBottom && useRepl) Repl.merged = true
    if (!(newSt ⊑ oldSt))
      npMap += np -> (oldSt ⊔ newSt)
      worklist += np

  /** get string for result of control points */
  def getString(
    cp: ControlPoint,
    color: Option[String] = None,
    detail: Boolean = false,
  ): String = ???

  /** logging the current analysis result */
  def logging: Unit = ???

  // ---------------------------------------------------------------------------
  // Implementation for AstFlowAnalyzer
  // ---------------------------------------------------------------------------
}
