package esmeta.analyzer

import esmeta.{ANALYZE_LOG_DIR, LINE_SEP}
import esmeta.analyzer.domain.*
import esmeta.cfg.*
import esmeta.error.*
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
  cfg: CFG,
  config: TypeAnalyzer.Config = TypeAnalyzer.Config(),
) extends Analyzer(cfg) {
  import TypeAnalyzer.*

  // record type mismatches
  def addMismatch(mismatch: TypeMismatch): Unit =
    mismatchMap += mismatch.ap -> mismatch
  lazy val mismatches: Set[TypeMismatch] = mismatchMap.values.toSet
  private var mismatchMap: Map[AnalysisPoint, TypeMismatch] = Map()

  /** type semantics as results */
  class Semantics(npMap: Map[NodePoint[Node], AbsState])
    extends AbsSemantics(npMap) {

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

  /** abstract transfer function for types */
  trait Transfer extends AbsTransfer {
    import AbsState.monad.{Result => ResultMonad, _}
    type Result = ResultMonad

    override def returnIfAbrupt(
      riaExpr: EReturnIfAbrupt,
      value: AbsValue,
      check: Boolean,
    )(using cp: ControlPoint): Result[AbsValue] = {
      val checkReturn: Result[Unit] =
        if (check) doReturn(riaExpr, value.abruptCompletion)
        else if (!value.abruptCompletion.isBottom)
          val riap = ReturnIfAbruptPoint(cp, riaExpr)
          addMismatch(UncheckedAbruptCompletionMismatch(riap, value.ty))
        else ()
      for (_ <- checkReturn) yield value.unwrapCompletion
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
      cp: CallPoint[Node],
      args: List[AbsValue],
      cont: Boolean,
      method: Boolean,
    ): Map[Local, AbsValue] = {
      val CallPoint(callerNp, calleeNp) = cp
      val callee: Func = calleeNp.func
      // get parameters
      val params: List[Param] = callee.irFunc.params
      // check arity
      val arity @ (from, to) = callee.arity
      val len = args.length
      if (config.arity && (len < from || to < len))
        addMismatch(ArityMismatch(cp, len))
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
                addMismatch(ParamTypeMismatch(aap, argTy))
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
      val InternalReturnPoint(irReturn, ReturnPoint(callee, view)) = irp
      // wrap completion by conditions specified in
      // [5.2.3.5 Implicit Normal Completion]
      // (https://tc39.es/ecma262/#sec-implicit-normal-completion)
      val newRet =
        if (callee.isReturnComp) givenRet.wrapCompletion else givenRet
      val givenTy = newRet.value.ty.removeAbsent
      val expected = callee.retTy.ty match
        case _: UnknownTy        => newRet
        case expectedTy: ValueTy =>
          // return type check when it is a known type
          if (config.returnType && !(givenTy <= expectedTy))
            addMismatch(ReturnTypeMismatch(irp, givenTy))
          AbsRet(AbsValue(expectedTy))
      super.doReturn(irp, expected)
  }

  /** transfer function */
  object transfer extends Transfer

  /** perform type analysis for given targets */
  def apply(
    targets: List[Func],
    ignore: Ignore,
    log: Boolean,
    silent: Boolean,
  ): Semantics = apply(
    init = Semantics(initNpMap(targets)),
    postProcess = (sem: Semantics) => {
      if (log) logging(sem)
      var unusedSet = ignore.names
      val detected = mismatches.filter(mismatch => {
        val name = mismatch.func.name
        unusedSet -= name
        !ignore.names.contains(name)
      })
      if (!detected.isEmpty || !unusedSet.isEmpty)
        val app = new Appender
        // show detected type mismatches
        if (!detected.isEmpty)
          app :> "* " >> detected.size
          app >> " type mismatches are detected."
        // show unused names
        if (!unusedSet.isEmpty)
          app :> "* " >> unusedSet.size
          app >> " names are not used to ignore mismatches."
        detected.toList.map(_.toString).sorted.map(app :> _)
        // show help message about how to use the ignorance system
        ignore.filename.map(path =>
          if (ignore.update)
            dumpJson(
              name = "algorithm names for the ignorance system",
              data = mismatches.map(_.func.name).toList.sorted,
              filename = path,
              noSpace = false,
            )
          else
            app :> "=" * 80
            app :> "To suppress this error message, "
            app >> "add/remove the following names to `" >> path >> "`:"
            detected.map(_.func.name).toList.sorted.map(app :> "  + " >> _)
            unusedSet.toList.sorted.map(app :> "  - " >> _)
            app :> "=" * 80,
        )
        throw TypeCheckFail(if (silent) None else Some(app.toString))
      sem
    },
  )
  def apply(
    target: Option[String],
    ignore: Ignore,
    log: Boolean,
    silent: Boolean,
  ): Semantics =
    val targets = getInitTargets(target, silent)
    if (!silent) println(s"- ${targets.size} functions are initial targets.")
    apply(targets, ignore, log, silent)

  // all entry node points
  def getNps(targets: List[Func]): List[NodePoint[Node]] = for {
    func <- targets
    entry = func.entry
    view = getView(func)
  } yield NodePoint(func, entry, view)

  // get initial abstract states in each node point
  def initNpMap(targets: List[Func]): Map[NodePoint[Node], AbsState] = (for {
    np @ NodePoint(func, _, _) <- getNps(targets)
    st = getState(func)
  } yield np -> st).toMap

  // get view from a function
  def getView(func: Func): View = View()

  // get initial state of function
  def getState(func: Func): AbsState = func.params.foldLeft(AbsState.Empty) {
    case (st, Param(x, ty, opt, _)) =>
      var v = AbsValue(ty.ty)
      if (opt) v ⊔= AbsValue.absentTop
      st.update(x, v)
  }

  /** find initial analysis targets based on a given regex pattern */
  def getInitTargets(
    target: Option[String],
    silent: Boolean = false,
  ): List[Func] =
    // find all possible initial analysis target functions
    val allFuncs = cfg.funcs.filter(_.isParamTysDefined)
    target.fold(allFuncs)(pattern => {
      val funcs = allFuncs.filter(f => pattern.r.matches(f.name))
      if (!silent && funcs.isEmpty)
        warn(s"failed to find functions matched with the pattern `$pattern`.")
      funcs
    })

  // logging mode
  def logging(sem: Semantics): Unit = {
    mkdir(ANALYZE_LOG_DIR)
    dumpFile(
      name = "type analysis result",
      data = sem.typesString,
      filename = s"$ANALYZE_LOG_DIR/types",
    )
    dumpFile(
      name = "visiting counter for control points",
      data = sem.counter.toList
        .sortBy(_._2)
        .map { case (cp, k) => s"[$k] $cp" }
        .mkString(LINE_SEP),
      filename = s"$ANALYZE_LOG_DIR/counter",
    )
    dumpFile(
      name = "detected type mismatches",
      data = mismatches.toList
        .map(_.toString)
        .sorted
        .mkString(LINE_SEP),
      filename = s"$ANALYZE_LOG_DIR/mismatches",
    )
  }
}
object TypeAnalyzer {
  // set type domains
  initDomain(
    stateDomain = state.TypeDomain,
    valueDomain = value.TypeDomain,
    retDomain = ret.TypeDomain,
  )

  // no sensitivity
  IR_SENS = false

  // use type refinement
  USE_REFINE = true

  /** algorithm names used in ignoring type mismatches */
  case class Ignore(
    filename: Option[String] = None,
    names: Set[String] = Set(),
    update: Boolean = false,
  )
  object Ignore:
    def apply(filename: String, update: Boolean): Ignore = Ignore(
      filename = Some(filename),
      names = optional { readJson[Set[String]](filename) }.getOrElse(Set()),
      update = update,
    )

  /** configuration for type checking */
  case class Config(
    // type checkers
    arity: Boolean = true,
    paramType: Boolean = true,
    returnType: Boolean = true,
    uncheckedAbrupt: Boolean = false,
  )
}
