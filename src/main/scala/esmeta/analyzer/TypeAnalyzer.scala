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
  val config: TypeAnalyzer.Config = TypeAnalyzer.Config(),
) extends Analyzer(cfg) {
  import TypeAnalyzer.*

  // record type mismatches
  def addMismatch(mismatch: TypeMismatch): Unit =
    val ap = if (TY_SENS) mismatch.ap.withoutView else mismatch.ap
    mismatchMap += ap -> mismatch
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

    /** loading monads */
    import AbsState.monad.*

    /** return-if-abrupt completion */
    override def returnIfAbrupt(
      riaExpr: EReturnIfAbrupt,
      value: AbsValue,
      check: Boolean,
    )(using cp: ControlPoint): Result[AbsValue] = {
      if (config.uncheckedAbrupt && !check && !value.abruptCompletion.isBottom)
        val riap = ReturnIfAbruptPoint(cp, riaExpr)
        addMismatch(UncheckedAbruptCompletionMismatch(riap, value.ty))
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
        case retTy: ValueTy if calleeFunc.isParamTysDefined && !TY_SENS =>
          for {
            nextNode <- call.next
            nextNp = NodePoint(callerFunc, nextNode, View())
            retV = AbsValue(retTy)
            newSt = callerSt.defineLocal(call.lhs -> retV)
          } sem += nextNp -> newSt
        // Otherwise, do original abstract call semantics
        case _ =>
          super.doCall(callerNp, callerSt, calleeFunc, args, captured, method)

    /** call transition */
    override def getCalleeEntries(
      callerNp: NodePoint[Call],
      callerSt: AbsState,
      calleeFunc: Func,
      args: List[AbsValue],
      captured: Map[Name, AbsValue],
      method: Boolean,
    ): List[(NodePoint[_], AbsState)] =
      for {
        baseView <- if (TY_SENS) getViewFromArgs(args) else List(View())
      } yield {
        // handle ir callsite sensitivity
        val NodePoint(callerFunc, callSite, callerView) = callerNp

        val calleeNp = NodePoint(calleeFunc, calleeFunc.entry, baseView)
        val calleeSt = callerSt.copied(locals =
          getLocals(
            CallPoint(callerNp, calleeNp),
            args,
            cont = false,
            method,
          ) ++ captured,
        )
        (calleeNp, calleeSt)
      }

    private def getViewFromArgs(args: List[AbsValue]): List[View] =
      val types = getTypes(args.map(_.ty))
      val views = types.map(t => View(tys = t))
      views

    private def getTypes(args: List[ValueTy]): List[List[ValueTy]] = {
      args.foldRight(List(List[ValueTy]())) {
        case (aty, tysList) =>
          for {
            tys <- tysList
            ty <- aty.gamma
          } yield ty :: tys
      }
    }

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
      val viewargs = calleeNp.view.tys.map { AbsValue(_) }
      val argsWithOptional = (if (TY_SENS) viewargs else args) ++ List.fill(
        to - len,
      )(AbsValue.absentTop)
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
          // force to set expected type for parameters when not type-sensitive
          val paramTy = if (TY_SENS) arg else expected
          param.lhs -> paramTy
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
      val retTy = if (TY_SENS) newRet else expected
      super.doReturn(irp, retTy)

    override def transfer(
      st: AbsState,
      bop: BOp,
      left: AbsValue,
      right: AbsValue,
    )(using cp: ControlPoint): AbsValue =
      import BOp.*
      import esmeta.interpreter.Interpreter
      (left.getSingle, right.getSingle) match {
        case (Zero, _) | (_, Zero) => AbsValue.Bot
        case (One(l: SimpleValue), One(r: SimpleValue)) =>
          optional(AbsValue(Interpreter.eval(bop, l, r)))
            .getOrElse(AbsValue.Bot)
        case (One(Math(l)), One(Math(r))) =>
          optional(AbsValue(Interpreter.eval(bop, Math(l), Math(r))))
            .getOrElse(AbsValue.Bot)
        case (One(lpart: Part), One(rpart: Part))
            if bop == Eq || bop == Equal =>
          if (lpart == rpart) {
            if (st.isSingle(lpart)) AVT
            else AVB
          } else AVF
        case (One(l), One(r)) if bop == Eq || bop == Equal =>
          AbsValue(l == r)
        case _ =>
          bop match {
            case Add | Sub | Div | Mul | Mod | UMod | Pow | Lt | Equal =>
              checkBOp(bop, left, right, Set(MathT, NumberT, BigIntT))
            case LShift | SRShift | URShift | BAnd | BOr | BXOr =>
              checkBOp(bop, left, right, Set(MathT, BigIntT))
            case _ =>
          }
          bop match {
            case BAnd    => left & right
            case BOr     => left | right
            case BXOr    => left ^ right
            case Eq      => left =^= right
            case Equal   => left ==^== right
            case Lt      => left < right
            case And     => left && right
            case Or      => left || right
            case Xor     => left ^^ right
            case Add     => left + right
            case Sub     => left sub right
            case Div     => left / right
            case Mul     => left * right
            case Mod     => left % right
            case UMod    => left %% right
            case Pow     => left ** right
            case LShift  => left << right
            case SRShift => left >> right
            case URShift => left >>> right
          }
      }

    private def checkBOp(
      op: BOp,
      l: AbsValue,
      r: AbsValue,
      check: Set[ValueTy],
    )(using cp: ControlPoint): Unit = {
      if (l.isBottom || r.isBottom) return
      val valid = check.exists(ty => {
        val lty = l.ty -- InfT
        val rty = r.ty -- InfT
        lty <= ty && rty <= ty
      })
      if (!valid) {
        val bop = BinaryOperationPoint(cp, op, l, r)
        addMismatch(BinaryOperatorTypeMismatch(bop))
      }
    }
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
  def getView(func: Func): View = {
    if (!TY_SENS) View()
    else {
      val paramTy = func.paramTys.map(_.ty)
      View(tys = paramTy.map(_ match {
        case _: UnknownTy =>
          throw Error("parameter call with UnknownTy is not allowed")
        case ty: ValueTy => ty
      }))
    }
  }

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
    val allFuncs =
      cfg.funcs.filter(f => f.isParamTysDefined && !f.isCloure && !f.isCont)
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
    arity: Boolean,
    paramType: Boolean,
    returnType: Boolean,
    uncheckedAbrupt: Boolean,
    invalidAstProperty: Boolean,
    invalidStrProperty: Boolean,
    invalidNameProperty: Boolean,
    invalidCompProperty: Boolean,
    invalidRecordProperty: Boolean,
    invalidListProperty: Boolean,
    invalidSymbolProperty: Boolean,
    invalidSubMapProperty: Boolean,
    invalidPropertyAccess: Boolean,
    propertyUpdate: Boolean,
    mapAlloc: Boolean,
  )
  object Config:
    val ignoreAll = Config(
      arity = false,
      paramType = false,
      returnType = false,
      uncheckedAbrupt = false,
      invalidAstProperty = false,
      invalidStrProperty = false,
      invalidNameProperty = false,
      invalidCompProperty = false,
      invalidRecordProperty = false,
      invalidListProperty = false,
      invalidSymbolProperty = false,
      invalidSubMapProperty = false,
      invalidPropertyAccess = false,
      propertyUpdate = false,
      mapAlloc = false,
    )

    def apply(): Config = Config(
      arity = 1 <= PRIORITY_FLAG,
      paramType = 1 <= PRIORITY_FLAG,
      returnType = 1 <= PRIORITY_FLAG,
      uncheckedAbrupt = 1 <= PRIORITY_FLAG,
      invalidAstProperty = 1 <= PRIORITY_FLAG,
      invalidStrProperty = 1 <= PRIORITY_FLAG,
      invalidNameProperty = 1 <= PRIORITY_FLAG,
      invalidCompProperty = 1 <= PRIORITY_FLAG,
      invalidRecordProperty = 1 <= PRIORITY_FLAG,
      invalidListProperty = 1 <= PRIORITY_FLAG,
      invalidSymbolProperty = 1 <= PRIORITY_FLAG,
      invalidSubMapProperty = 1 <= PRIORITY_FLAG,
      invalidPropertyAccess = 1 <= PRIORITY_FLAG,
      propertyUpdate = 1 <= PRIORITY_FLAG,
      mapAlloc = 1 <= PRIORITY_FLAG,
    )
}
