package esmeta.analyzer.astflow

import esmeta.{AST_FLOW_LOG_DIR, LINE_SEP}
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
  val targetPattern: Option[String] = None,
  val log: Boolean = false,
  val detail: Boolean = false,
  val silent: Boolean = false,
  override val useRepl: Boolean = false,
  override val replContinue: Boolean = false,
) extends Analyzer
  with AbsValueDecl
  with AbsStateDecl
  with AbsRetDecl
  with AbsTransferDecl
  with ViewDecl {

  val irStringifier = IRElem.getStringifier(false, false)
  import irStringifier.given

  // initialization
  npMap = getInitNpMap(targetFuncs)

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
  def getInitNpMap(
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
        val v = AbsValue(x.name)
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

  /** logging the current analysis result */
  def logging: Unit = {
    val time = elapsedTime

    // create log directory
    mkdir(AST_FLOW_LOG_DIR)

    // basic logging
    dumpFile(
      name = "summary of analysis",
      data = {
        var info = Vector[(String, Any)]()
        cfg.spec.version.map(v => info :+= "version" -> v.toString)
        info ++= Vector(
          "duration" -> f"${time}%,d ms",
          "iter" -> iter,
          "analyzed" -> Map(
            "funcs" -> ratioSimpleString(analyzedFuncs.size, cfg.funcs.size),
            "nodes" -> ratioSimpleString(analyzedNodes.size, cfg.nodes.size),
            "returns" -> ratioSimpleString(analyzedReturns.size, cfg.funcs.size),
          ),
        )
        Yaml(info: _*)
      },
      filename = s"$AST_FLOW_LOG_DIR/summary.yml",
      silent = silent,
    )
    dumpFile(
      name = "visiting counter for control points",
      data = counter.toList
        .sortBy(_._2)
        .map { case (cp, k) => s"[$k] $cp" }
        .mkString(LINE_SEP),
      filename = s"$AST_FLOW_LOG_DIR/counter",
      silent = silent,
    )

    // detailed logging
    if (detail)
      val unreachableDir = s"$AST_FLOW_LOG_DIR/unreachable"
      val unreachableFuncs = cfg.funcs.filterNot(analyzedFuncs.contains)
      val unreachableNodes = cfg.nodes.filterNot(analyzedNodes.contains)
      val unreachableReturns = cfg.funcs.filterNot(analyzedReturns.contains)

      // create unreachable directory
      mkdir(unreachableDir)

      dumpFile(
        name = "unreachable functions",
        data = unreachableFuncs.sorted.map(_.nameWithId).mkString(LINE_SEP),
        filename = s"$unreachableDir/funcs",
        silent = silent,
      )
      dumpFile(
        name = "unreachable nodes",
        data = unreachableNodes
          .groupBy(cfg.funcOf)
          .toList
          .sortBy(_._1)
          .map {
            case (f, ns) =>
              f.nameWithId +
              ns.sorted.map(LINE_SEP + "  " + _.name).mkString
          }
          .mkString(LINE_SEP),
        filename = s"$unreachableDir/nodes",
        silent = silent,
      )
      dumpFile(
        name = "unreachable function returns",
        data = unreachableReturns.sorted.map(_.nameWithId).mkString(LINE_SEP),
        filename = s"$unreachableDir/returns",
        silent = silent,
      )
      dumpFile(
        name = "detailed type analysis result for each control point",
        data = getStrings(detail = true).mkString(LINE_SEP),
        filename = s"$AST_FLOW_LOG_DIR/detailed-result",
        silent = silent,
      )
  }

  // ---------------------------------------------------------------------------
  // Implementation for AstFlowAnalyzer
  // ---------------------------------------------------------------------------
  /** all possible initial analysis target functions */
  def targetFuncs: List[Func] =
    val allFuncs = cfg.funcs.filter(f => f.isParamTysPrecise && !f.isCont)
    val funcs = targetPattern.fold(allFuncs)(pattern => {
      val funcs = allFuncs.filter(f => pattern.r.matches(f.name))
      if (!silent && funcs.isEmpty)
        warn(s"failed to find functions matched with the pattern `$pattern`.")
      funcs
    })
    if (!silent) println(s"- ${funcs.size} functions are initial targets.")
    funcs
}
