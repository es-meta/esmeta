package esmeta.analyzer.tychecker

import esmeta.{ANALYZE_LOG_DIR, LINE_SEP}
import esmeta.analyzer.*
import esmeta.cfg.*
import esmeta.es.*
import esmeta.ir.{Func => _, *, given}
import esmeta.ty.{*, given}
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*

/** specification type analyzer for ECMA-262 */
class TyChecker(
  val cfg: CFG,
  val targetPattern: Option[String] = None,
  val inferTypeGuard: Boolean = true,
  val useBooleanGuard: Boolean = false,
  val useProvenance: Boolean = false,
  val useSyntacticKill: Boolean = false,
  val noRefine: Boolean = false,
  val typeSens: Boolean = false,
  val config: TyChecker.Config = TyChecker.Config(),
  val ignore: TyChecker.Ignore = Ignore(),
  val log: Boolean = false,
  val detail: Boolean = false,
  val silent: Boolean = false,
  override val useRepl: Boolean = false,
  override val replContinue: Boolean = false,
) extends Analyzer
  with SymTyDecl
  with AbsValueDecl
  with AbsStateDecl
  with AbsRetDecl
  with AbsTransferDecl
  with TypeGuardDecl
  with ViewDecl
  with EffectDecl {

  val tyStringifier = TyElem.getStringifier(false, false)
  import tyStringifier.given

  npMap = getInitNpMap(targetFuncs)

  def isTypeGuardCandidate(func: Func): Boolean =
    !func.isBuiltin && !func.isSDO && !func.isAux && func.weakComplete

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
    val time = elapsedTime

    // create log directory
    mkdir(ANALYZE_LOG_DIR)

    // get type guards
    val typeGuards = getTypeGuards

    // basic logging
    dumpFile(
      name = "summary of type analysis",
      data = {
        var info = Vector[(String, Any)]()
        cfg.spec.version.map(v => info :+= "version" -> v.toString)
        info ++= Vector(
          "options" -> Map(
            "typeSens" -> typeSens,
            "inferTypeGuard" -> inferTypeGuard,
            "useProvenance" -> useProvenance,
            "useSyntacticKill" -> useSyntacticKill,
          ),
          "duration" -> f"${time}%,d ms",
          "error" -> errors.size,
          "iter" -> iter,
          "analyzed" -> Map(
            "funcs" -> ratioSimpleString(analyzedFuncs.size, cfg.funcs.size),
            "nodes" -> ratioSimpleString(analyzedNodes.size, cfg.nodes.size),
            "returns" -> ratioSimpleString(analyzedReturns.size, cfg.funcs.size),
          ),
        )
        if (detail)
          info :+= "refined" -> Map(
            "targets" -> refinedTargets,
            "locals" -> refinedLocals,
            "avg. depth" -> refinedAvgDepth,
          )
        if (detail && useProvenance)
          info :+= "provenance" -> Map(
            "size" -> provCnt,
            "avg. size" -> provAvgSize,
            "avg. depth" -> provAvgDepth,
            "avg. leaf" -> provAvgLeaf,
          )
        if (inferTypeGuard) info :+= "guards" -> typeGuards.size
        Yaml(info*)
      },
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
        .map(_.toString)
        .mkString(LINE_SEP + LINE_SEP),
      filename = s"$ANALYZE_LOG_DIR/errors",
      silent = silent,
    )
    dumpFile(
      name = "inline summary for evaluation in tsv format",
      data = Vector(
        this.iter, // iter
        time, // duration
        errors.size, // error
        this.analyzedFuncs.size, // analyzed funcs
        cfg.funcs.size, // total funcs
        this.analyzedNodes.size, // analyzed nodes
        cfg.nodes.size, // total nodes
        if (detail) refinedTargets else 0, // refined targets
        if (detail) refinedLocals else 0, // refined locals
        if (detail) refinedAvgDepth else 0, // refined avg. depth
        if (inferTypeGuard) typeGuards.size else 0, // guards
        if (detail && useProvenance) provCnt else 0, // provenance
        if (detail && useProvenance) provAvgSize else 0, // provenance avg. size
        if (detail && useProvenance) provAvgDepth
        else 0, // provenance avg. depth
        if (detail && useProvenance) provAvgLeaf else 0, // provenance avg. leaf
      ).mkString("\t"),
      filename = s"$ANALYZE_LOG_DIR/summary",
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
          .map(f =>
            f._1.nameWithId + f._2.sorted
              .map(n => s"$LINE_SEP  ${n.name}")
              .mkString,
          )
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
        data = errors.toList.sorted.map(_.point.node.id).mkString(LINE_SEP),
        filename = s"$ANALYZE_LOG_DIR/error-nodes",
        silent = silent,
      )
      dumpFile(
        name = "detailed type analysis result for each control point",
        data = getStrings(detail = true).mkString(LINE_SEP),
        filename = s"$ANALYZE_LOG_DIR/detailed-types",
        silent = silent,
      )
      dumpFile(
        name = "refined targets",
        data = refinedString,
        filename = s"$ANALYZE_LOG_DIR/refined",
        silent = silent,
      )
      if (inferTypeGuard) {
        import ProvPrinter.*

        dumpFile(
          name = "type guard information",
          data = typeGuards
            .sortBy { case (f, _) => f.id }
            .map { (f, v) => s"[${f.id}] ${f.name} -> $v" }
            .mkString(LINE_SEP),
          filename = s"$ANALYZE_LOG_DIR/guards",
          silent = silent,
        )
        if (useProvenance) {
          // ------------------------------------------------------------------
          // Split provenance logs by algorithm
          // ------------------------------------------------------------------
          // Group provenance entries by the algorithm (function) that owns the
          // refinement target.
          import scala.collection.mutable.{Map => MMap}

          val grouped
            : Map[Func, Map[(RefinementTarget, Base, ValueTy), Provenance]] =
            provenances
              .groupBy { case ((target, _, _), _) => target.func }
              .view
              .mapValues(_.toMap)
              .toMap

          // Re-use the same pretty-printer rule used in `provString` to
          // serialize each group independently.
          val provDir = s"$ANALYZE_LOG_DIR/provenance"
          dumpDir[
            (Func, String),
          ](
            name = "provenance information (per algorithm)",
            iterable = grouped.toList.sortBy(_._1.id).map {
              case (f, mp) => (f, formatProvenances(mp))
            },
            dirname = provDir,
            getName = { case (f, _) => s"${f.normalizedName}" },
            getData = { case (_, data) => data },
            remove = true,
            silent = silent,
          )

          // Keep a combined file for backward compatibility
          dumpFile(
            name = "provenance information (all)",
            data = provString,
            filename = s"$ANALYZE_LOG_DIR/provenance-logs",
            silent = silent,
          )
          dumpFile(
            name = "provenance graph",
            data = sizeAndDepth,
            filename = s"$ANALYZE_LOG_DIR/provenance-size-depth",
            silent = silent,
          )
          dumpFile(
            name = "provenance graph",
            data = depthAndLeaf,
            filename = s"$ANALYZE_LOG_DIR/provenance-depth-leaf",
            silent = silent,
          )
          dumpFile(
            name = "provenance graph",
            data = sizeAndLeaf,
            filename = s"$ANALYZE_LOG_DIR/provenance-size-leaf",
            silent = silent,
          )
        }
        if (useSyntacticKill) {
          dumpFile(
            name = "mutated locals",
            data = cfg.funcs
              .map(f => f.nameWithId -> f.mutableLocals.mkString(", "))
              .mkString(LINE_SEP),
            filename = s"$ANALYZE_LOG_DIR/mutated",
            silent = silent,
          )
          dumpFile(
            name = "impure functions",
            data = impureFuncs.map(_.name).toList.sorted.mkString(LINE_SEP),
            filename = s"$ANALYZE_LOG_DIR/impure",
            silent = silent,
          )
          dumpFile(
            name = "pure functions",
            data = (cfg.funcs.map(_.name).toSet -- impureFuncs.map(
              _.name,
            )).toList.sorted.mkString(LINE_SEP),
            filename = s"$ANALYZE_LOG_DIR/pure",
            silent = silent,
          )
        }
      }
  }

  /** refined targets */
  var refined: Map[RefinementTarget, (Set[Local], Int)] = Map()
  def refinedTargets: Int = refined.size
  def refinedLocals: Int = refined.values.map(_._1.size).sum
  def refinedAvgDepth: Double =
    refined.values.map(_._2).sum.toDouble / refined.size
  def refinedString: String =
    given Rule[Map[RefinementTarget, (Set[Local], Int)]] =
      (app, refined) =>
        val sorted = refined.toList.sortBy { (t, _) => t }
        for ((target, (xs, depth)) <- sorted)
          app >> target >> " -> "
          app >> xs.toList.sorted.mkString("[locals: ", ", ", "]")
          app >> " [depth: " >> depth >> "]"
          app >> LINE_SEP
        app
    (new Appender >> refined).toString

  /** -----------------------------------------------------------------
    * Pretty-printing for provenance information in human-readable form
    * -----------------------------------------------------------------
    */
  private def nodeStepStr(nd: Node): String =
    nd.loc match
      case Some(loc) if loc.steps.nonEmpty => s"Step ${loc.stepString}"
      case Some(_)                         => "Step 0"
      case None                            => "Unknown step"

  private def formatProvenances(
    m: Map[(RefinementTarget, Base, ValueTy), Provenance],
  ): String =
    import esmeta.ir.util.Stringifier
    val irStringifier = IRElem.getStringifier(false, false)
    import irStringifier.elemRule

    val app = new Appender
    val sorted = m.toList.sortBy {
      case ((target, _, ty), _) =>
        (target.func.id, target.node.id, ty.toString)
    }
    for (((target, base, ty), prov) <- sorted) {
      val algo = target.func.name
      val stepStr = nodeStepStr(target.node)
      // Base string
      val baseStr = base.toString

      app >> s"`$baseStr` in the $stepStr of $algo to $ty" >> LINE_SEP

      val inner = prov match
        case RefinePoint(_, child) => child
        case _                     => prov
      app >> inner.toTree(1) >> LINE_SEP >> LINE_SEP
    }
    app.toString

  /** provenance information */
  var provenances: Map[(RefinementTarget, Base, ValueTy), Provenance] = Map()
  def provCnt = provenances.size
  def provAvgSize = provenances.values.map(_.size).sum.toDouble / provCnt
  def provAvgDepth = provenances.values.map(_.depth).sum.toDouble / provCnt
  def provAvgLeaf = provenances.values.map(_.leafCnt).sum.toDouble / provCnt
  def provString: String = formatProvenances(provenances)
  def provList = provenances.values.toList
  def sizeAndDepth = provList
    .map(p => (p.size, p.depth))
    .groupMapReduce(identity)(_ => 1)(_ + _)
    .map { case ((size, depth), cnt) => s"$size,$depth,$cnt" }
    .mkString(LINE_SEP)
  def depthAndLeaf = provList
    .map(p => (p.depth, p.leafCnt))
    .groupMapReduce(identity)(_ => 1)(_ + _)
    .map { case ((depth, leaf), cnt) => s"$depth,$leaf,$cnt" }
    .mkString(LINE_SEP)
  def sizeAndLeaf = provList
    .map(p => (p.size, p.leafCnt))
    .groupMapReduce(identity)(_ => 1)(_ + _)
    .map { case ((size, leaf), cnt) => s"$size,$leaf,$cnt" }
    .mkString(LINE_SEP)

  /** inferred type guards */
  def getTypeGuards: List[(Func, AbsValue)] =
    import SymTy.*, SymExpr.*
    for {
      func <- cfg.funcs
      entrySt = getResult(NodePoint(func, func.entry, emptyView))
      AbsRet(value, _) = getResult(ReturnPoint(func, emptyView))
      if value.hasTypeGuard(entrySt)
      guard = TypeGuard(for {
        (dty, pred) <- value.guard.map
        newPred = TypeConstr(for {
          pair <- pred.map
          (x, (ty, prov)) = pair
          if !(entrySt.getTy(x) <= ty)
        } yield pair)
        if newPred.nonTop
      } yield dty -> newPred)
    } yield func -> value.copy(guard = guard)

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

  /** update internal map */
  def +=(pair: (NodePoint[Node], AbsState)): Unit =
    val (np, newSt) = pair
    val oldSt = getResult(np)
    if (!oldSt.isBottom && useRepl) Repl.merged = true
    if (!newSt.hasBottom && !(newSt ⊑ oldSt))
      npMap += np -> (oldSt ⊔ newSt)
      worklist += np

  /** get callee state */
  def getCalleeState(
    callerSt: AbsState,
    locals: List[(Local, AbsValue)],
    callee: Func,
  ): AbsState =
    import SymExpr.*, SymTy.*
    given AbsState = callerSt
    if (inferTypeGuard) {
      val idxLocals = locals.zipWithIndex
      val (newLocals, symEnv) = (for {
        ((x, value), sym) <- idxLocals
      } yield {
        if useSyntacticKill && callee.canMakeSideEffect then
          (x -> AbsValue(STy(value.ty)), sym -> ValueTy.Bot)
        else (x -> AbsValue(SSym(sym)), sym -> value.ty)
      }).unzip
      AbsState(true, newLocals.toMap, symEnv.toMap, TypeConstr(), Effect())
    } else AbsState(true, locals.toMap, Map(), TypeConstr(), Effect())

  /** get initial abstract states in each node point */
  private def getInitNpMap(
    targets: List[Func],
  ): Map[NodePoint[Node], AbsState] = (for {
    func <- targets
    entry = func.entry
    (view, st) <- getViewWithSt(func)
    np = NodePoint(func, entry, view)
  } yield np -> st).toMap

  private def getViewWithSt(func: Func): List[(View, AbsState)] =
    val pairs = func.params.map {
      case Param(x, ty, _, _) => x -> ty.ty.toValue
    }
    val locals = pairs.map { (x, v) => x -> AbsValue(v) }
    val view = if (typeSens) View(pairs.map(_._2)) else emptyView
    List(view -> getCalleeState(AbsState.Empty, locals, func))

  /** initialization of ECMAScript environment */
  lazy val init: Initialize = cfg.init

  /** global environment */
  lazy val base: Map[Global, AbsValue] = for {
    (x, (_, t)) <- init.initTypedGlobal.toMap
  } yield x -> AbsValue(t.toValue)

  /** arguments information for each callsite */
  var argsInfo: Map[NodePoint[Call], List[AbsValue]] = Map()

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
      val (param, ty) = pair
      app >> param >> ": " >> ty
    (new Appender >> cfg.funcs.toList.sortBy(_.name)).toString

  /** For Expriement: Imitating Kent's work */

  lazy val synCallGraph: Map[Func, Set[Func]] =
    cfg.funcs
      .map { func =>
        val callees = func.nodes.flatMap {
          case call: Call =>
            call.inst.fold(Set[Func]()) {
              case ICall(_, fexpr, _) =>
                fexpr match
                  case EClo(fname, _) => cfg.funcs.filter(_.name == fname)
                  case ECont(fname)   => cfg.funcs.filter(_.name == fname)
                  case _              => Set()
              case ISdoCall(_, base, _, _) =>
                base match
                  case EClo(fname, _) => cfg.funcs.filter(_.name == fname)
                  case ECont(fname)   => cfg.funcs.filter(_.name == fname)
                  case _              => Set()
              case _ => Set()
            }
          case _ => Set()
        }
        for callee <- callees yield callee -> func
      }
      .flatMap(identity)
      .groupMap(_._1)(_._2)
      .map((k, v) => k -> v.toSet)

  lazy val impureFuncs =
    def basicImpureFuncs: Set[Func] =
      cfg.funcs.filter(_.mutableLocals.nonEmpty).toSet
    var visited = basicImpureFuncs
    val queue = scala.collection.mutable.Queue.from(basicImpureFuncs)
    while queue.nonEmpty do
      val func = queue.dequeue()
      for callee <- synCallGraph.getOrElse(func, Set()) do
        if !visited.contains(callee) then
          visited += callee
          queue.enqueue(callee)
    println(
      s"${visited.size} functions are impure while ${cfg.funcs.size} functions exist.",
    )
    visited

  extension (inst: NormalInst) {
    def mutable: Set[Local] =
      def toBase(ref: Ref): Option[Local] = ref match
        case Field(base, expr) => toBase(base)
        case l: Local          => Some(l)
        case _                 => None
      inst match
        case IAssign(ref, _)                => toBase(ref).toSet
        case IPush(_, ERef(list: Local), _) => Set(list)
        // case IExpand(base, expr) => XXX: Unsound
        // case IDelete(base, expr) => XXX: Unsound
        case _ => Set()
  }
  extension (node: Node) {
    def mutable: Set[Local] = node match
      case block: Block => block.insts.flatMap(_.mutable).toSet
      case _            => Set()
  }
  extension (func: Func) {
    def mutableLocals: Set[Base] = func.nodes.flatMap(_.mutable)
    def canMakeSideEffect = impureFuncs.contains(func)
  }
  extension (np: NodePoint[?]) {
    def isMutable(ref: Ref): Boolean = ref match
      case l: Local => np.func.mutableLocals.contains(l)
      case _        => false
    def canMakeSideEffect: Boolean = np.func.canMakeSideEffect
  }
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
    checkUnaryOp: Boolean = true,
    checkBinaryOp: Boolean = true,
  )
