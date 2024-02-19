package esmeta.analyzer

import esmeta.{ANALYZE_LOG_DIR, LINE_SEP}
import esmeta.analyzer.domain.*
import esmeta.cfg.*
import esmeta.es.*
import esmeta.ir.{Func => IRFunc, *}
import esmeta.ty.*
import esmeta.ty.util.{Stringifier => TyStringifier}
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.state.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*

/** specification type analyzer for ECMA-262 */
class TypeAnalyzer(
  val cfg: CFG,
  val targetPattern: Option[String] = None,
  val tySens: Boolean = false,
  val config: TypeAnalyzer.Config = TypeAnalyzer.Config(),
  val ignore: TypeAnalyzer.Ignore = Ignore(),
  val log: Boolean = false,
  val detail: Boolean = false,
  val silent: Boolean = false,
  override val useRepl: Boolean = false,
  override val replContinue: Boolean = false,
) extends Analyzer {
  import TypeAnalyzer.*

  /** perform type analysis */
  lazy val analyze: Unit =
    AbsState.setBase(new Initialize(cfg))
    transfer.fixpoint
    if (log) logging

  /** unused ignore set */
  protected var _unusedSet: Set[String] = ignore.names
  inline def unusedSet: Set[String] = _unusedSet

  /** perform type analysis with the given control flow graph */
  lazy val errors: Set[TypeError] = errorMap.values.toSet
  lazy val detected = errors.filter(error => {
    val name = error.func.name
    _unusedSet -= name
    !ignore.names.contains(name)
  })

  /** all possible initial analysis target functions */
  def targetFuncs: List[Func] =
    val allFuncs = cfg.funcs.filter(_.isParamTysDefined)
    val funcs = targetPattern.fold(allFuncs)(pattern => {
      val funcs = allFuncs.filter(f => pattern.r.matches(f.name))
      if (!silent && funcs.isEmpty)
        warn(s"failed to find functions matched with the pattern `$pattern`.")
      funcs
    })
    if (!silent) println(s"- ${funcs.size} functions are initial targets.")
    funcs

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

  /** no sensitivity */
  override val irSens: Boolean = false

  /** use type refinement */
  override val useRefine: Boolean = true

  /** type semantics as results */
  lazy val sem: Semantics = new Semantics
  class Semantics extends AbsSemantics(getInitNpMap(targetFuncs)) {

    /** type analysis result string */
    def typesString: String =
      given getRule: Rule[Iterable[Func]] = (app, funcs) =>
        import TyStringifier.given
        given Rule[Iterable[(String, ValueTy)]] = iterableRule("(", ", ", ")")
        app >> "-" * 80
        for (func <- funcs) {
          val rp = ReturnPoint(func, View())
          app :> "   " >> func.headString
          val fname = func.name
          val entryNp = NodePoint(func, func.entry, View())
          val st = this(entryNp)
          val newParams =
            for (p <- func.params) yield p.lhs.name -> st.get(p.lhs, entryNp).ty
          app :> "-> " >> "def "
          app >> func.irFunc.kind.toString >> fname >> newParams
          app >> ": " >> rpMap.get(rp).fold(func.retTy.ty)(_.value.ty)
          app :> "-" * 80
        }
        app
      given paramRule: Rule[(String, ValueTy)] = (app, pair) =>
        import TyStringifier.given
        val (param, ty) = pair
        app >> param
        if (ty.absent) app >> "?"
        app >> ": " >> ty -- AbsentT
      (new Appender >> cfg.funcs.toList.sortBy(_.name)).toString
  }

  /** transfer function */
  lazy val transfer: Transfer = new Transfer
  class Transfer extends AbsTransfer {

    /** loading monads */
    import AbsState.monad.*

    /** return-if-abrupt completion */
    override def returnIfAbrupt(
      riaExpr: EReturnIfAbrupt,
      value: AbsValue,
      check: Boolean,
    )(using np: NodePoint[Node]): Result[AbsValue] = {
      if (config.uncheckedAbrupt && !check && !value.abruptCompletion.isBottom)
        val riap = ReturnIfAbruptPoint(np, riaExpr)
        addError(UncheckedAbruptError(riap, value.ty))
      super.returnIfAbrupt(riaExpr, value, check)
    }

    /** handle calls */
    override def doCall(
      callerNp: NodePoint[Call],
      callerSt: AbsState,
      calleeFunc: Func,
      args: List[AbsValue],
      captured: Map[Name, AbsValue] = Map(),
      method: Boolean = false,
    ): Unit =
      val NodePoint(callerFunc, call, view) = callerNp
      calleeFunc.retTy.ty match
        // Stop the propagation of analysis when it is unnecessary to analyze
        // the callee function because it has full type annotations.
        case retTy: ValueTy if calleeFunc.isParamTysDefined =>
          for {
            nextNode <- call.next
            nextNp = NodePoint(callerFunc, nextNode, View())
            retV = AbsValue(retTy)
            newSt = callerSt.defineLocal(call.lhs -> retV)
          } sem += nextNp -> newSt
        // Otherwise, do original abstract call semantics
        case _ =>
          super.doCall(callerNp, callerSt, calleeFunc, args, captured, method)

    /** get local variables */
    override def getLocals(
      cp: CallPoint,
      args: List[AbsValue],
      cont: Boolean,
      method: Boolean,
    ): Map[Local, AbsValue] = {
      val CallPoint(callerNp, callee) = cp
      // get parameters
      val params: List[Param] = callee.irFunc.params
      // check arity
      val arity @ (from, to) = callee.arity
      val len = args.length
      if (config.arity && (len < from || to < len))
        addError(ArityMismatch(cp, len))
      // fill optional args after arity checked
      val argsWithOptional = args ++ List.fill(to - len)(AbsValue.absentTop)
      // construct local type environment
      (for (((param, arg), idx) <- (params zip argsWithOptional).zipWithIndex)
        yield {
          val expected = param.ty.ty match
            case _: UnknownTy => arg
            case paramTy: ValueTy =>
              val argTy = arg.ty.removeAbsent
              if (method && idx == 0) {
                () /* ignore `this` for method-like calls */
              } else if (config.paramType && !(argTy <= paramTy))
                val aap = ArgAssignPoint(cp, idx)
                addError(ParamTypeMismatch(aap, argTy))
              AbsValue(paramTy)
          // force to set expected type for parameters
          param.lhs -> expected
        }).toMap
    }

    /** update return points */
    override def doReturn(
      irp: InternalReturnPoint,
      givenRet: AbsRet,
    ): Unit =
      val InternalReturnPoint(NodePoint(func, _, view), irReturn) = irp
      // wrap completion by conditions specified in
      // [5.2.3.5 Implicit Normal Completion]
      // (https://tc39.es/ecma262/#sec-implicit-normal-completion)
      val newRet =
        if (func.isReturnComp) givenRet.wrapCompletion else givenRet
      val givenTy = newRet.value.ty.removeAbsent
      val expected = func.retTy.ty match
        case _: UnknownTy        => newRet
        case expectedTy: ValueTy =>
          // return type check when it is a known type
          if (config.returnType && !(givenTy <= expectedTy))
            addError(ReturnTypeMismatch(irp, givenTy))
          AbsRet(AbsValue(expectedTy))
      super.doReturn(irp, expected)
  }

  /** use type abstract domains */
  stateDomain = Some(StateTypeDomain)
  retDomain = Some(RetTypeDomain)
  valueDomain = Some(ValueTypeDomain)

  /** conversion to string */
  override def toString: String =
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

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------

  /** record type errors */
  private def addError(error: TypeError): Unit =
    errorMap += error.point -> error
  private var errorMap: Map[AnalysisPoint, TypeError] = Map()

  /** all entry node points */
  private def getNps(targets: List[Func]): List[NodePoint[Node]] = for {
    func <- targets
    entry = func.entry
    view = getView(func)
  } yield NodePoint(func, entry, view)

  /** get initial abstract states in each node point */
  private def getInitNpMap(
    targets: List[Func],
  ): Map[NodePoint[Node], AbsState] =
    (for {
      np @ NodePoint(func, _, _) <- getNps(targets)
      st = getState(func)
    } yield np -> st).toMap

  /** get view from a function */
  private def getView(func: Func): View = View()

  /** get initial state of function */
  private def getState(func: Func): AbsState =
    func.params.foldLeft(AbsState.Empty) {
      case (st, Param(x, ty, opt, _)) =>
        var v = AbsValue(ty.ty)
        if (opt) v ⊔= AbsValue.absentTop
        st.update(x, v)
    }

  /** logging mode */
  private def logging: Unit = {
    val analyzedFuncs = sem.analyzedFuncs
    val analyzedNodes = sem.analyzedNodes
    val analyzedReturns = sem.analyzedReturns

    // create log directory
    mkdir(ANALYZE_LOG_DIR)

    // basic logging
    dumpFile(
      name = "summary of type analysis",
      data = Yaml(
        "duration" -> f"${sem.elapsedTime}%,d ms",
        "error" -> errors.size,
        "iter" -> sem.iter,
        "analyzed" -> Map(
          "funcs" -> ratioSimpleString(analyzedFuncs.size, cfg.funcs.size),
          "nodes" -> ratioSimpleString(analyzedNodes.size, cfg.nodes.size),
          "returns" -> ratioSimpleString(analyzedReturns.size, cfg.funcs.size),
        ),
      ),
      filename = s"$ANALYZE_LOG_DIR/summary.yml",
    )
    dumpFile(
      name = "type analysis result for each function",
      data = sem.typesString,
      filename = s"$ANALYZE_LOG_DIR/types",
      silent = silent,
    )
    dumpFile(
      name = "visiting counter for control points",
      data = sem.counter.toList
        .sortBy(_._2)
        .map { case (cp, k) => s"[$k] $cp" }
        .mkString(LINE_SEP),
      filename = s"$ANALYZE_LOG_DIR/counter",
      silent = silent,
    )
    dumpFile(
      name = "detected type errors",
      data = errors.toList.map(_.toString).sorted.mkString(LINE_SEP),
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
      )
      dumpFile(
        name = "unreachable nodes",
        data = unreachableNodes
          .groupBy(cfg.funcOf)
          .toList
          .sortBy(_._1)
          .map {
            case (f, ns) =>
              f.nameWithId + ns.map(LINE_SEP + "  " + _.name).mkString
          }
          .mkString(LINE_SEP),
        filename = s"$unreachableDir/nodes",
      )
      dumpFile(
        name = "unreachable function returns",
        data = unreachableReturns.sorted.map(_.nameWithId).mkString(LINE_SEP),
        filename = s"$unreachableDir/returns",
      )
      dumpFile(
        name = "detailed type analysis result for each control point",
        data = sem.resultStrings(detail = true).mkString(LINE_SEP),
        filename = s"$ANALYZE_LOG_DIR/detailed-types",
      )
  }
}
object TypeAnalyzer:

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
    // type checkers
    arity: Boolean = true,
    paramType: Boolean = true,
    returnType: Boolean = true,
    uncheckedAbrupt: Boolean = false,
  )
