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
import scala.collection.mutable.{Map => MMap}

/** specification type analyzer for ECMA-262 */
class TypeAnalyzer(
  cfg: CFG,
  alarmLevel: Int = 1,
) extends Analyzer(cfg) { analyzer =>
  import TypeAnalyzer.*

  /** configuration for type errors */
  val config: AlarmConfig = AlarmConfig(alarmLevel = alarmLevel)

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

    /** refine invalid base for property reference */
    override def refinePropBase(
      cp: ControlPoint,
      prop: Prop,
      base: AbsValue,
    ): AbsValue =
      val baseTy = base.ty
      val noPropTy = baseTy.noProp
      if (config.invalidPropBase && !noPropTy.isBottom)
        sem += InvalidPropBase(PropBasePoint(PropPoint(cp, prop)), baseTy)
      AbsValue(baseTy -- noPropTy)

    /** return-if-abrupt completion */
    override def returnIfAbrupt(
      riaExpr: EReturnIfAbrupt,
      value: AbsValue,
      check: Boolean,
    )(using cp: ControlPoint): Result[AbsValue] =
      if (config.uncheckedAbruptComp)
        if (!check && !value.abruptCompletion.isBottom)
          val riaPoint = ReturnIfAbruptPoint(cp, riaExpr)
          sem += UncheckedAbruptComp(riaPoint, value.ty)
      super.returnIfAbrupt(riaExpr, value, check)

    /** call transition */
    override def getCalleeView(
      callerNp: NodePoint[Call],
      calleeFunc: Func,
      args: List[AbsValue],
    ): List[View] = List(View()) // TODO

    /** assign argument to parameter */
    override def assignArg(
      cp: CallPoint,
      method: Boolean,
      idx: Int,
      param: Param,
      arg: AbsValue,
    ): AbsValue = param.ty.ty match
      case _: UnknownTy => arg
      case paramTy: ValueTy =>
        val argTy = arg.ty
        val normalArgTy = argTy.removeAbsent
        if (method && idx == 0) () /* ignore `this` for method calls */
        else if (config.paramTypeMismatch && !(normalArgTy <= paramTy))
          sem += ParamTypeMismatch(
            ArgAssignPoint(cp, idx),
            normalArgTy,
          )
        AbsValue(argTy && paramTy)

    /** get return value with a state */
    override def getReturn(irp: InternalReturnPoint, ret: AbsRet): AbsRet =
      irp.func.retTy.ty match
        case _: UnknownTy => ret
        case expectedTy: ValueTy =>
          val givenTy = ret.value.ty.removeAbsent
          // return type check when it is a known type
          if (config.returnTypeMismatch && !(givenTy <= expectedTy))
            sem += ReturnTypeMismatch(irp, givenTy)
          ret.copy(value = AbsValue(givenTy && expectedTy))

    /** transfer function for unary operators */
    override def transfer(
      st: AbsState,
      unary: EUnary,
      operand: AbsValue,
    )(using cp: ControlPoint): AbsValue =
      import UOp.*
      if (config.unaryOpTypeMismatch)
        val operandTy = operand.ty
        unary.uop match
          case Abs | Floor =>
            checkUnary(unary, operandTy, MathT)
          case Neg | BNot =>
            checkUnary(unary, operandTy, MathT || NumberT || BigIntT)
          case Not =>
            checkUnary(unary, operandTy, BoolT)
      super.transfer(st, unary, operand)

    private def checkUnary(
      unary: EUnary,
      operandTy: ValueTy,
      expectedTys: ValueTy,
    )(using cp: ControlPoint): Unit = if (!(operandTy <= expectedTys))
      sem += UnaryOpTypeMismatch(
        UnaryOpPoint(cp, unary),
        operandTy,
      )

    /** transfer function for binary operators */
    override def transfer(
      st: AbsState,
      binary: EBinary,
      left: AbsValue,
      right: AbsValue,
    )(using cp: ControlPoint): AbsValue =
      import BOp.*
      if (config.binaryOpTypeMismatch)
        val (lhsTy, rhsTy) = (left.ty, right.ty)
        binary.bop match
          case Add | Sub | Mul | Pow | Div | UMod | Mod | Lt | Equal =>
            checkBinary(binary, lhsTy, rhsTy, Set(ExtMathT, NumberT, BigIntT))
          case LShift | SRShift | URShift | BAnd | BOr | BXOr =>
            checkBinary(binary, lhsTy, rhsTy, Set(MathT, BigIntT))
          case And | Or | Xor =>
            checkBinary(binary, lhsTy, rhsTy, Set(BoolT))
          case Eq =>
      super.transfer(st, binary, left, right)

    private def checkBinary(
      binary: EBinary,
      lhsTy: ValueTy,
      rhsTy: ValueTy,
      expectedTys: Set[ValueTy],
    )(using cp: ControlPoint): Unit =
      if (!expectedTys.exists(ty => lhsTy <= ty || rhsTy <= ty))
        sem += BinaryOpTypeMismatch(
          BinaryOpPoint(cp, binary),
          lhsTy,
          rhsTy,
        )
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
      val errors: Set[TypeError] = sem.errors
      if (log) logging(sem, errors)
      val ignoreNames = ignore.names
      val errorNames = errors.map(_.func.name)
      val unusedNames = ignoreNames -- errorNames
      val detectedNames = errorNames -- ignoreNames
      val detected = errors.filter(detectedNames contains _.func.name)
      if (!detectedNames.isEmpty || !unusedNames.isEmpty)
        val app = new Appender
        // show detected type mismatches
        if (!detected.isEmpty)
          app :> "* " >> detected.size
          app >> " type mismatches are detected."
        // show unused names
        if (!unusedNames.isEmpty)
          app :> "* " >> unusedNames.size
          app >> " names are not used to ignore mismatches."
        detected.toList.map(_.toString).sorted.map(app :> _)
        // update ignore file or guide user to update it
        ignore.filename.map(path =>
          if (ignore.update)
            dumpJson(
              name = "algorithm names for the ignorance system",
              data = errorNames.toList.sorted,
              filename = path,
              noSpace = false,
            )
          else
            app :> "=" * 80
            if (!detectedNames.isEmpty)
              app :> "To suppress this error message, "
              app >> "add the following names to `" >> path >> "`:"
              detectedNames.toList.sorted.map(app :> "  + " >> _)
            if (!unusedNames.isEmpty)
              app :> "To suppress this error message, "
              app >> "remove the following names to `" >> path >> "`:"
              unusedNames.toList.sorted.map(app :> "  - " >> _)
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
    val targets = getInitTargets(cfg, target, silent)
    if (!silent) println(s"- ${targets.size} functions are initial targets.")
    apply(targets, ignore, log, silent)

  // logging mode
  private def logging(sem: Semantics, errors: Set[TypeError]): Unit = {
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
      name = "detected type errors",
      data = errors.toList
        .map(_.toString)
        .sorted
        .mkString(LINE_SEP),
      filename = s"$ANALYZE_LOG_DIR/errors",
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

  /** configuration for type error alarms */
  case class AlarmConfig(
    paramTypeMismatch: Boolean = false,
    returnTypeMismatch: Boolean = false,
    uncheckedAbruptComp: Boolean = false,
    invalidPropBase: Boolean = false,
    unaryOpTypeMismatch: Boolean = false,
    binaryOpTypeMismatch: Boolean = false,
  )
  object AlarmConfig:
    val PARAM_TYPE_MISMATCH_LEVEL = 1
    val RETURN_TYPE_MISMATCH_LEVEL = 1
    val UNCHECKED_ABRUPT_COMP_LEVEL = 2
    val INVALID_PROP_BASE_LEVEL = 2
    val UNARY_OP_TYPE_MISMATCH_LEVEL = 2
    val BINARY_OP_TYPE_MISMATCH_LEVEL = 2

    def apply(alarmLevel: Int): AlarmConfig = AlarmConfig(
      paramTypeMismatch = alarmLevel >= PARAM_TYPE_MISMATCH_LEVEL,
      returnTypeMismatch = alarmLevel >= RETURN_TYPE_MISMATCH_LEVEL,
      uncheckedAbruptComp = alarmLevel >= UNCHECKED_ABRUPT_COMP_LEVEL,
      invalidPropBase = alarmLevel >= INVALID_PROP_BASE_LEVEL,
      unaryOpTypeMismatch = alarmLevel >= UNARY_OP_TYPE_MISMATCH_LEVEL,
      binaryOpTypeMismatch = alarmLevel >= BINARY_OP_TYPE_MISMATCH_LEVEL,
    )

  // get initial abstract states in each node point
  def initNpMap(targets: List[Func]): Map[NodePoint[Node], AbsState] = (for {
    np @ NodePoint(func, _, _) <- getNps(targets)
    st = getState(func)
  } yield np -> st).toMap

  // all entry node points
  def getNps(targets: List[Func]): List[NodePoint[Node]] = for {
    func <- targets
    entry = func.entry
    view = getView(func)
  } yield NodePoint(func, entry, view)

  // get view from a function
  def getView(func: Func): View = View() // TODO

  // get initial state of function
  def getState(func: Func): AbsState = func.params.foldLeft(AbsState.Empty) {
    case (st, Param(x, ty, opt, _)) =>
      var v = AbsValue(ty.ty)
      if (opt) v ⊔= AbsValue.absentTop
      st.update(x, v)
  }

  /** find initial analysis targets based on a given regex pattern */
  def getInitTargets(
    cfg: CFG,
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
}
