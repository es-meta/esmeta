package esmeta.analyzer.tychecker

import esmeta.{ANALYZE_LOG_DIR, LINE_SEP}
import esmeta.analyzer.*
import esmeta.cfg.*
import esmeta.es.*
import esmeta.ir.{Func => _, *, given}
import esmeta.ty.*
import esmeta.ty.util.{Stringifier => TyStringifier}
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*

/** specification type analyzer for ECMA-262 */
class TyChecker(
  val cfg: CFG,
  val targetPattern: Option[String] = None,
  val useTypeGuard: Boolean = true,
  val config: TyChecker.Config = TyChecker.Config(),
  val ignore: TyChecker.Ignore = Ignore(),
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
  with TypeErrorDecl
  with TypeGuardDecl
  with ViewDecl {

  npMap = getInitNpMap(targetFuncs)

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
    // create log directory
    mkdir(ANALYZE_LOG_DIR)

    // basic logging
    dumpFile(
      name = "summary of type analysis",
      data = Yaml(
        "duration" -> f"${elapsedTime}%,d ms",
        "error" -> errors.size,
        "iter" -> iter,
        "analyzed" -> Map(
          "funcs" -> ratioSimpleString(analyzedFuncs.size, cfg.funcs.size),
          "nodes" -> ratioSimpleString(analyzedNodes.size, cfg.nodes.size),
          "returns" -> ratioSimpleString(analyzedReturns.size, cfg.funcs.size),
        ),
      ),
      filename = s"$ANALYZE_LOG_DIR/summary.yml",
      silent = silent,
    )
    dumpFile(
      name = "type analysis result for each function",
      data = typesString,
      filename = s"$ANALYZE_LOG_DIR/types",
      silent = silent,
    )
    dumpFile(
      name = "visiting counter for control points",
      data = counter.toList
        .sortBy(_._2)
        .map { case (cp, k) => s"[$k] $cp" }
        .mkString(LINE_SEP),
      filename = s"$ANALYZE_LOG_DIR/counter",
      silent = silent,
    )
    dumpFile(
      name = "detected type errors",
      data = errors.toList.sorted
        .map(_.toString(detail = true))
        .mkString(LINE_SEP + LINE_SEP),
      filename = s"$ANALYZE_LOG_DIR/errors",
      silent = silent,
    )

    // detailed logging
    if (detail)
      val unreachableDir = s"$ANALYZE_LOG_DIR/unreachable"
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
        name = "node ids for error points",
        data = errors.toList.map(_.point.node.id).sorted.mkString(LINE_SEP),
        filename = s"$ANALYZE_LOG_DIR/error-nodes",
        silent = silent,
      )
      dumpFile(
        name = "detailed type analysis result for each control point",
        data = getStrings(detail = true).mkString(LINE_SEP),
        filename = s"$ANALYZE_LOG_DIR/detailed-types",
        silent = silent,
      )
  }

  // ---------------------------------------------------------------------------
  // Implementation for Type Checker
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

  /** all entry node points */
  private def getNps(targets: List[Func]): List[NodePoint[Node]] = for {
    func <- targets
    entry = func.entry
    view = getView(func)
  } yield NodePoint(func, entry, view)

  /** update internal map */
  def +=(pair: (NodePoint[Node], AbsState)): Unit =
    val (np, newSt) = pair
    val oldSt = getResult(np)
    if (!oldSt.isBottom && useRepl) Repl.merged = true
    if (!newSt.hasBottom && !(newSt ⊑ oldSt))
      npMap += np -> (oldSt ⊔ newSt)
      worklist += np

  /** get view from a function */
  private def getView(func: Func): View = emptyView

  /** get initial state of function */
  private def getState(func: Func): AbsState =
    val locals = func.params.map {
      case Param(x, ty, _, _) => x -> AbsValue(ty.ty.toValue)
    }
    getCalleeState(AbsState.Empty, locals)

  /** get callee state */
  def getCalleeState(
    callerSt: AbsState,
    locals: List[(Local, AbsValue)],
  ): AbsState =
    import SymExpr.*, SymRef.*
    given AbsState = callerSt
    if (useTypeGuard) {
      val idxLocals = locals.zipWithIndex
      val (newLocals, symEnv) = (for {
        ((x, value), sym) <- idxLocals
      } yield (
        x -> AbsValue(BotT, One(SERef(SBase(sym))), TypeGuard()),
        sym -> value.ty,
      )).unzip
      AbsState(true, newLocals.toMap, symEnv.toMap, SymPred())
    } else AbsState(true, locals.toMap, Map(), SymPred())

  /** get initial abstract states in each node point */
  private def getInitNpMap(
    targets: List[Func],
  ): Map[NodePoint[Node], AbsState] =
    (for {
      np @ NodePoint(func, _, _) <- getNps(targets)
      st = getState(func)
    } yield np -> st).toMap

  /** initialization of ECMAScript environment */
  lazy val init: Initialize = new Initialize(cfg)

  /** global environment */
  lazy val base: Map[Global, AbsValue] = for {
    (x, (_, t)) <- init.initTypedGlobal.toMap
  } yield x -> AbsValue(t.toValue)

  /** arguments information for each callsite */
  protected var argsInfo: Map[NodePoint[Call], List[(Expr, AbsValue)]] = Map()

  /** unused ignore set */
  protected var _unusedSet: Set[String] = ignore.names
  inline def unusedSet: Set[String] = _unusedSet

  /** detected type errors */
  def errors: Set[TypeError] = errorMap.values.toSet
  protected def addError(error: TypeError): Unit =
    errorMap += error.point -> error
  private var errorMap: Map[TypeErrorPoint, TypeError] = Map()

  /** detected type errors after filtering with ignore set */
  def detected = errors.filter(error => {
    val name = error.func.name
    _unusedSet -= name
    !ignore.names.contains(name)
  })

  /** check if the ignore set needs to be updated */
  def needUpdate: Boolean = detected.nonEmpty || unusedSet.nonEmpty

  /** update ignorance system */
  def updateIgnore: Unit = for (path <- ignore.filename)
    dumpJson(
      name = "algorithm names for the ignorance system",
      data = errors.map(_.func.name).toList.sorted,
      filename = path,
      noSpace = false,
      silent = silent,
    )

  /** conversion to string */
  def getMessage: String =
    val app = new Appender
    // show detected type errors
    if (detected.nonEmpty)
      app :> "* " >> detected.size
      app >> " type errors are detected."
    // show unused names
    if (unusedSet.nonEmpty)
      app :> "* " >> unusedSet.size
      app >> " names are not used to ignore errors."
    detected.toList.map(_.toString).sorted.map(app :> _)
    // show help message about how to use the ignorance system
    for (path <- ignore.filename)
      app :> "=" * 80
      if (detected.nonEmpty)
        app :> "To suppress this error message, "
        app >> "add the following names to `" >> path >> "`:"
        detected.map(_.func.name).toList.sorted.map(app :> "  + " >> _)
      if (unusedSet.nonEmpty)
        app :> "To suppress this error message, "
        app >> "remove the following names from `" >> path >> "`:"
      unusedSet.toList.sorted.map(app :> "  - " >> _)
      app :> "=" * 80
    app.toString

  /** type analysis result string */
  def typesString: String =
    given getRule: Rule[Iterable[Func]] = (app, funcs) =>
      import TyStringifier.given
      given Rule[Iterable[(String, ValueTy)]] = iterableRule("(", ", ", ")")
      app >> "-" * 80
      for (func <- funcs) {
        val rp = ReturnPoint(func, emptyView)
        app :> "   " >> func.headString
        val fname = func.name
        val entryNp = NodePoint(func, func.entry, emptyView)
        val st = getResult(entryNp)
        given AbsState = st
        val newParams =
          for (p <- func.params) yield p.lhs.name -> st.get(p.lhs).ty
        app :> "-> " >> "def "
        app >> func.irFunc.kind.toString >> fname >> newParams
        app >> ": " >> rpMap.get(rp).fold(func.retTy.ty)(_.value.ty)
        app :> "-" * 80
      }
      app
    given paramRule: Rule[(String, ValueTy)] = (app, pair) =>
      import TyStringifier.given
      val (param, ty) = pair
      app >> param >> ": " >> ty
    (new Appender >> cfg.funcs.toList.sortBy(_.name)).toString
}

object TyChecker:

  /** algorithm names used in ignoring type errors */
  case class Ignore(
    filename: Option[String] = None,
    names: Set[String] = Set(),
  )
  object Ignore:
    def apply(filename: String): Ignore = Ignore(
      filename = Some(filename),
      names = optional { readJson[Set[String]](filename) }.getOrElse(Set()),
    )

  /** configuration for type checking */
  case class Config(
    checkArity: Boolean = true,
    checkParamType: Boolean = true,
    checkReturnType: Boolean = true,
    checkUncheckedAbrupt: Boolean = false, // TODO
    checkInvalidBase: Boolean = false, // TODO
    checkUnaryOp: Boolean = true,
    checkBinaryOp: Boolean = true,
  )
