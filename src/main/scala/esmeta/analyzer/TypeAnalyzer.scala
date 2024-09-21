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
  val typeSens: Boolean = false, // TODO
  val useTypeGuard: Boolean = true,
  val config: TypeAnalyzer.Config = TypeAnalyzer.Config(),
  val ignore: TypeAnalyzer.Ignore = Ignore(),
  val log: Boolean = false,
  val detail: Boolean = false,
  val silent: Boolean = false,
  override val useRepl: Boolean = false,
  override val replContinue: Boolean = false,
) extends Analyzer {
  import TypeAnalyzer.*

  /** initialization of ECMAScript environment */
  lazy val init: Initialize = new Initialize(cfg)

  /** perform type analysis */
  lazy val analyze: Unit =
    AbsState.setBase(init)
    transfer.fixpoint
    if (log) logging

  /** unused ignore set */
  protected var _unusedSet: Set[String] = ignore.names
  inline def unusedSet: Set[String] = _unusedSet

  /** perform type analysis with the given control flow graph */
  def errors: Set[TypeError] = errorMap.values.toSet
  def detected = errors.filter(error => {
    val name = error.func.name
    _unusedSet -= name
    !ignore.names.contains(name)
  })

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

  /** transfer function */
  lazy val transfer: Transfer = new Transfer
  class Transfer extends AbsTransfer {

    /** loading monads */
    import AbsState.monad.*

    /** assign argument to parameter */
    override def assignArg(
      callPoint: CallPoint,
      method: Boolean,
      idx: Int,
      param: Param,
      arg: AbsValue,
    ): AbsValue =
      val paramTy = param.ty.ty.toValue
      val argTy = arg.ty
      if (method && idx == 0) () /* ignore `this` for method calls */
      else if (config.checkParamType && !(argTy <= paramTy))
        addError(ParamTypeMismatch(ArgAssignPoint(callPoint, idx), argTy))
      AbsValue(paramTy && argTy)

    /** callee entries */
    override def getCalleeEntries(
      callerNp: NodePoint[Call],
      locals: List[(Local, AbsValue)],
    ): List[(View, List[(Local, AbsValue)])] = List(View() -> (for {
      (local, value) <- locals
    } yield local -> AbsValue(value.ty)))

    /** get local variables */
    override def getLocals(
      callPoint: CallPoint,
      method: Boolean,
      vs: List[AbsValue],
    ): List[(Local, AbsValue)] =
      val CallPoint(callerNp, callee) = callPoint
      val arity @ (from, to) = callee.arity
      val len = vs.length
      if (config.checkArity && (len < from || to < len))
        addError(ArityMismatch(callPoint, len))
      super.getLocals(callPoint, method, vs)

    /** check if the return type can be used */
    private lazy val canUseReturnTy: Func => Boolean = cached { func =>
      !func.retTy.isImprec ||
      (useTypeGuard && typeGuards.contains(func.name)) ||
      defaultTypeGuards.contains(func.name)
    }

    /** handle calls */
    override def doCall(
      callPoint: CallPoint,
      callerSt: AbsState,
      args: List[Expr],
      vs: List[AbsValue],
      captured: Map[Name, AbsValue] = Map(),
      method: Boolean = false,
      tgt: Option[NodePoint[Node]] = None,
    ): Unit =
      val CallPoint(callerNp, callee) = callPoint
      if (canUseReturnTy(callee)) {
        val call = callerNp.node
        val retTy = callee.retTy.ty
        val xs = args.map {
          case ERef(x: Local) => Some(x)
          case _              => None
        }
        val newRetV = (for {
          guard <- typeGuards.get(callee.name)
          if useTypeGuard || defaultTypeGuards.contains(callee.name)
        } yield guard(xs, vs, retTy)).getOrElse(AbsValue(retTy))
        for {
          nextNp <- getAfterCallNp(callerNp)
          newSt = callerSt.define(call.lhs, newRetV)
        } sem += nextNp -> newSt
      }
      super.doCall(callPoint, callerSt, args, vs, captured, method, tgt)

    /** propagate callee analysis result */
    override def propagate(rp: ReturnPoint, callerNp: NodePoint[Call]): Unit =
      if (!canUseReturnTy(rp.func))
        val AbsRet(value, st) = sem(rp)
        (for {
          nextNp <- getAfterCallNp(callerNp)
          if !value.isBottom
          callerSt = sem.callInfo(callerNp)
        } yield sem += nextNp -> st.doReturn(
          callerSt,
          callerNp.node.lhs,
          value,
        )).getOrElse(super.propagate(rp, callerNp))

    /** transfer function for return points */
    override def apply(rp: ReturnPoint): Unit =
      if (!canUseReturnTy(rp.func)) super.apply(rp)

    /** default type guards */
    val defaultTypeGuards: Set[String] = Set(
      "__APPEND_LIST__",
      "__FLAT_LIST__",
      "__GET_ITEMS__",
      "__CLAMP__",
      "Completion",
      "NormalCompletion",
    )

    /** type guards */
    type TypeGuard = (List[Option[Local]], List[AbsValue], Ty) => AbsValue
    val typeGuards: Map[String, TypeGuard] =
      import RefinementKind.*
      Map(
        "__APPEND_LIST__" -> { (xs, vs, retTy) =>
          AbsValue(vs(0).ty || vs(1).ty, Map())
        },
        "__FLAT_LIST__" -> { (xs, vs, retTy) =>
          AbsValue(vs(0).ty.list.elem, Map())
        },
        "__GET_ITEMS__" -> { (xs, vs, retTy) =>
          val ast = vs(1).ty.toValue.grammarSymbol match
            case Fin(set) => AstT(set.map(_.name))
            case Inf      => AstT
          AbsValue(ListT(ast), Map())
        },
        "__CLAMP__" -> { (xs, vs, retTy) =>
          val refined =
            if (vs(0).ty.toValue <= (IntT || InfinityT))
              if (vs(1).ty.toValue <= MathT(0)) NonNegIntT
              else IntT
            else retTy
          AbsValue(refined, Map())
        },
        "Completion" -> { (xs, vs, retTy) =>
          AbsValue(vs(0).ty && CompT, Map())
        },
        "NormalCompletion" -> { (xs, vs, retTy) =>
          AbsValue(NormalT(vs(0).ty -- CompT), Map())
        },
        "IteratorClose" -> { (xs, vs, retTy) =>
          AbsValue(vs(1).ty || ThrowT, Map())
        },
        "AsyncIteratorClose" -> { (xs, vs, retTy) =>
          AbsValue(vs(1).ty || ThrowT, Map())
        },
        "OrdinaryObjectCreate" -> { (xs, vs, retTy) =>
          AbsValue(RecordT("OrdinaryObject"), Map())
        },
        "UpdateEmpty" -> { (xs, vs, retTy) =>
          val record = vs(0).ty.record
          val valueField = record("Value").value
          val updated = record.update(
            "Value",
            vs(1).ty || (valueField -- EnumT("empty")),
            refine = false,
          )
          AbsValue(ValueTy(record = updated), Map())
        },
        "MakeBasicObject" -> { (xs, vs, retTy) =>
          AbsValue(RecordT("Object"), Map())
        },
        "Await" -> { (xs, vs, retTy) =>
          AbsValue(NormalT(ESValueT) || ThrowT, Map())
        },
        "IsCallable" -> { (xs, vs, retTy) =>
          var map: Refinements = Map()
          xs(0).map { x => map += True -> Map(x -> RecordT("FunctionObject")) }
          AbsValue(retTy, map)
        },
        "IsConstructor" -> { (xs, vs, retTy) =>
          var map: Refinements = Map()
          xs(0).map { x => map += True -> Map(x -> RecordT("Constructor")) }
          AbsValue(retTy, map)
        },
        "RequireInternalSlot" -> { (xs, vs, retTy) =>
          var map: Refinements = Map()
          val refined = vs(1).ty.str.getSingle match
            case One(f) =>
              ValueTy(
                record = ObjectT.record.update(f, Binding.Exist, refine = true),
              )
            case _ => ObjectT
          xs(0).map { x => map += Normal -> Map(x -> refined) }
          AbsValue(retTy, map)
        },
        "ValidateTypedArray" -> { (xs, vs, retTy) =>
          var map: Refinements = Map()
          xs(0).map { x =>
            map += Normal -> Map(x -> RecordT("IntegerIndexedExoticObject"))
          }
          AbsValue(retTy, map)
        },
        "ValidateNonRevokedProxy" -> { (xs, vs, retTy) =>
          var map: Refinements = Map()
          xs(0).map { x =>
            map += Normal -> Map(
              x -> ValueTy.from(
                "Record[ProxyExoticObject { ProxyHandler : Record[Object], ProxyTarget : Record[Object] }]",
              ),
            )
          }
          AbsValue(retTy, map)
        },
        "IsPromise" -> { (xs, vs, retTy) =>
          var map: Refinements = Map()
          xs(0).map { x => map += True -> Map(x -> RecordT("Promise")) }
          AbsValue(retTy, map)
        },
        "IsRegExp" -> { (xs, vs, retTy) =>
          var map: Refinements = Map()
          xs(0).map { x =>
            map += NormalTrue -> Map(x -> ObjectT)
            map += Abrupt -> Map(x -> ObjectT)
          }
          AbsValue(retTy, map)
        },
        "NewPromiseCapability" -> { (xs, vs, retTy) =>
          var map: Refinements = Map()
          xs(0).map { x => map += Normal -> Map(x -> RecordT("Constructor")) }
          AbsValue(retTy, map)
        },
        "CreateListFromArrayLike" -> { (xs, vs, retTy) =>
          AbsValue((for {
            v <- vs.lift(1)
            str = v.ty.list.elem.str
            ss <- str match
              case Inf     => None
              case Fin(ss) => Some(ss)
            ty = ss.map(ValueTy.fromTypeOf).foldLeft(BotT)(_ || _)
            refined = retTy.toValue && NormalT(ListT(ty))
          } yield refined).getOrElse(retTy))
        },
        "IsUnresolvableReference" -> { (xs, vs, retTy) =>
          var map: Refinements = Map()
          xs(0).map { x =>
            map += True -> Map(
              x -> RecordT(
                "ReferenceRecord",
                Map("Base" -> EnumT("unresolvable")),
              ),
            )
            map += False -> Map(
              x -> RecordT(
                "ReferenceRecord",
                Map("Base" -> (ESValueT || RecordT("EnvironmentRecord"))),
              ),
            )
          }
          AbsValue(retTy, map)
        },
        "IsPropertyReference" -> { (xs, vs, retTy) =>
          var map: Refinements = Map()
          xs(0).map { x =>
            map += True -> Map(
              x -> RecordT("ReferenceRecord", Map("Base" -> ESValueT)),
            )
          }
          xs(0).map { x =>
            map += False -> Map(
              x -> RecordT(
                "ReferenceRecord",
                Map(
                  "Base" ->
                  (RecordT("EnvironmentRecord") || EnumT("unresolvable")),
                ),
              ),
            )
          }
          AbsValue(retTy, map)
        },
        "IsSuperReference" -> { (xs, vs, retTy) =>
          var map: Refinements = Map()
          xs(0).map { x =>
            map += True -> Map(x -> RecordT("SuperReferenceRecord"))
          }
          AbsValue(retTy, map)
        },
        "IsPrivateReference" -> { (xs, vs, retTy) =>
          var map: Refinements = Map()
          xs(0).map { x =>
            map += True -> Map(
              x -> RecordT(
                "ReferenceRecord",
                Map("ReferencedName" -> RecordT("PrivateName")),
              ),
            )
            map += False -> Map(
              x -> RecordT(
                "ReferenceRecord",
                Map(
                  "ReferencedName" -> (SymbolT || StrT /* TODO ESValue in latest version */ ),
                ),
              ),
            )
          }
          AbsValue(retTy, map)
        },
      )

    /** update return points */
    override def doReturn(
      irp: InternalReturnPoint,
      newRet: AbsRet,
    ): Unit =
      val InternalReturnPoint(NodePoint(func, _, view), irReturn) = irp
      val givenTy = newRet.value.ty
      val expected = func.retTy.ty match
        case _: UnknownTy        => newRet
        case expectedTy: ValueTy =>
          // return type check when it is a known type
          if (config.checkReturnType && !(givenTy <= expectedTy))
            addError(ReturnTypeMismatch(irp, givenTy))
          AbsRet(AbsValue(givenTy && expectedTy))
      super.doReturn(irp, expected)

    /** transfer function for normal instructions */
    override def transfer(inst: NormalInst)(using np: NodePoint[_]): Updater =
      inst match
        case IAssign(Field(x: Var, EStr(f)), expr) =>
          for {
            v <- transfer(expr)
            ty <- get(_.get(x).ty)
            record = ty.record.update(f, v.ty, refine = false)
            _ <- modify(_.update(x, AbsValue(ty.copied(record = record))))
          } yield ()
        case _ => super.transfer(inst)

    /** transfer function for expressions */
    override def transfer(
      expr: Expr,
    )(using np: NodePoint[Node]): Result[AbsValue] = expr match
      // a precise type of `the active function object` in built-in functions
      case ERef(
            Field(
              Field(Global("EXECUTION_STACK"), EMath(0)),
              EStr("Function"),
            ),
          ) if np.func.isBuiltin =>
        AbsValue(RecordT("Constructor"))
      // a precise type for intrinsic objects
      case ERef(
            Field(
              Field(
                Field(
                  Field(Global("EXECUTION_STACK"), EMath(0)),
                  EStr("Realm"),
                ),
                EStr("Intrinsics"),
              ),
              EStr(name),
            ),
          ) =>
        AbsValue(cfg.init.intr.kinds.getOrElse(name, ObjectT))
      case EMap((kty, vty), _) => AbsValue(MapT(kty.toValue, vty.toValue))
      case _                   => super.transfer(expr)

    /** transfer function for unary operators */
    override def transfer(
      st: AbsState,
      unary: EUnary,
      operand: AbsValue,
    )(using np: NodePoint[Node]): AbsValue =
      import UOp.*
      if (config.checkUnaryOp)
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
    )(using np: NodePoint[Node]): Unit = if (!(operandTy <= expectedTys))
      addError(UnaryOpTypeMismatch(UnaryOpPoint(np, unary), operandTy))

    /** transfer function for binary operators */
    override def transfer(
      st: AbsState,
      binary: EBinary,
      left: AbsValue,
      right: AbsValue,
    )(using np: NodePoint[Node]): AbsValue =
      import BOp.*
      if (config.checkBinaryOp)
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
    )(using np: NodePoint[Node]): Unit =
      if (!expectedTys.exists(ty => lhsTy <= ty || rhsTy <= ty))
        val binaryPoint = BinaryOpPoint(np, binary)
        addError(BinaryOpTypeMismatch(binaryPoint, lhsTy, rhsTy))

    /** refine condition */
    override def refine(
      cond: Expr,
      positive: Boolean,
    )(using np: NodePoint[_]): Updater = cond match {
      // refine boolean local variables
      case ERef(x: Local) =>
        refineBool(x, positive)
      // refine inequality
      case EBinary(BOp.Lt, l, r) =>
        refineIneq(l, r, positive)
      // refine local variables
      case EBinary(BOp.Eq, ERef(x: Local), expr) =>
        refineLocal(x, expr, positive)
      // refine field equality
      case EBinary(BOp.Eq, ERef(Field(x: Local, EStr(field))), expr) =>
        refineField(x, field, expr, positive)
      // refine field existence
      case EExists(Field(x: Local, EStr(field))) =>
        refineExistField(x, field, positive)
      // refine types
      case EBinary(BOp.Eq, ETypeOf(ERef(x: Local)), expr) =>
        refineType(x, expr, positive)
      // refine type checks
      case ETypeCheck(ERef(ref), ty) =>
        refineTypeCheck(ref, ty.ty.toValue, positive)
      // refine logical negation
      case EUnary(UOp.Not, e) =>
        refine(e, !positive)
      // refine logical disjunction
      case EBinary(BOp.Or, l, r) =>
        st =>
          if (positive) refine(l, true)(st) ⊔ refine(r, true)(st)
          else refine(r, false)(refine(l, false)(st))
      // refine logical conjunction
      case EBinary(BOp.And, l, r) =>
        st =>
          if (positive) refine(r, true)(refine(l, true)(st))
          else refine(l, false)(st) ⊔ refine(r, false)(st)
      // no pruning
      case _ => st => st
    }

    /** refine types */
    def refine(
      value: AbsValue,
      refinedValue: AbsValue,
    )(using np: NodePoint[_]): Result[List[Unit]] =
      import RefinementKind.*
      val refined = refinedValue.ty
      join(for {
        map <- value.refinements.collect {
          case (True, map) if refined <= TrueT                  => map
          case (False, map) if refined <= FalseT                => map
          case (Normal, map) if refined <= NormalT              => map
          case (Abrupt, map) if refined <= AbruptT              => map
          case (NormalTrue, map) if refined <= NormalT(TrueT)   => map
          case (NormalFalse, map) if refined <= NormalT(FalseT) => map
        }
        (x, ty) <- map
      } yield for {
        origV <- get(_.get(x))
        _ <- modify(_.update(x, AbsValue(ty) ⊓ origV))
      } yield ())

    /** refine types for boolean local variables */
    def refineBool(
      x: Local,
      positive: Boolean,
    )(using np: NodePoint[_]): Updater = for {
      l <- transfer(x)
      lv <- transfer(l)
      refinedV = if (positive) AVT else AVF
      _ <- modify(_.update(l, refinedV))
      _ <- refine(lv, refinedV)
    } yield ()

    /** refine types with inequalities */
    def refineIneq(
      l: Expr,
      r: Expr,
      positive: Boolean,
    )(using np: NodePoint[_]): Updater =
      def toLocal(e: Expr): Option[Local] = e match
        case ERef(x: Local) => Some(x)
        case _              => None
      for {
        lv <- transfer(l)
        rv <- transfer(r)
        lmath = lv.ty.math
        rmath = rv.ty.math
        _ <- modify { st =>
          val lst = toLocal(l).fold(st) { x =>
            var math = lmath
            var infinity = lv.ty.infinity --
              (if (positive) InfinityTy.Pos else InfinityTy.Neg)
            val refined = (r, rmath) match
              case (EMath(0), _) =>
                math = if (positive) NegIntTy else NonNegIntTy
              case l =>
            st.update(
              x,
              AbsValue(
                ValueTy(
                  math = math,
                  infinity = infinity,
                  number = lv.ty.number,
                  bigInt = lv.ty.bigInt,
                ),
              ),
            )
          }
          toLocal(r).fold(lst) { x =>
            var math = rmath
            var infinity = rv.ty.infinity --
              (if (positive) InfinityTy.Neg else InfinityTy.Pos)
            val refined = (l, lmath) match
              case (EMath(0), _) =>
                math = if (positive) PosIntTy else NonPosIntTy
              case _ => rmath
            lst.update(
              x,
              AbsValue(
                ValueTy(
                  math = math,
                  infinity = infinity,
                  number = rv.ty.number,
                  bigInt = rv.ty.bigInt,
                ),
              ),
            )
          }
        }
      } yield ()

    /** refine types of local variables with equality */
    def refineLocal(
      x: Local,
      expr: Expr,
      positive: Boolean,
    )(using np: NodePoint[_]): Updater = for {
      rv <- transfer(expr)
      l <- transfer(x)
      lv <- transfer(l)
      refinedV =
        if (positive) lv ⊓ rv
        else if (rv.isSingle) lv -- rv
        else lv
      _ <- modify(_.update(l, refinedV))
      _ <- refine(lv, refinedV)
    } yield ()

    /** TODO refine types with field equality */
    def refineField(
      x: Local,
      field: String,
      expr: Expr,
      positive: Boolean,
    )(using np: NodePoint[_]): Updater = for {
      rv <- transfer(expr)
      _ <- refineField(x, field, Binding(rv.ty), positive)
    } yield ()

    def refineField(
      x: Local,
      field: String,
      rbinding: Binding,
      positive: Boolean,
    )(using np: NodePoint[_]): Updater = for {
      l <- transfer(x)
      lv <- transfer(l)
      lty = lv.ty
      binding = if (positive) rbinding else lty.record(field) -- rbinding
      refinedTy = ValueTy(
        ast = lty.ast,
        record = lty.record.update(field, binding, refine = true),
      )
      _ <- modify(_.update(l, AbsValue(refinedTy)))
    } yield ()

    /** refine types with field existence */
    def refineExistField(
      x: Local,
      field: String,
      positive: Boolean,
    )(using np: NodePoint[_]): Updater =
      refineField(x, field, Binding.Exist, positive)

    /** refine types with `typeof` constraints */
    def refineType(
      x: Local,
      expr: Expr,
      positive: Boolean,
    )(using np: NodePoint[_]): Updater = for {
      l <- transfer(x)
      lv <- transfer(l)
      rv <- transfer(expr)
      lty = lv.ty
      rty = rv.ty
      refinedV = rty.str.getSingle match
        case One(tname) =>
          val value = AbsValue(ValueTy.fromTypeOf(tname))
          if (positive) lv ⊓ value else lv -- value
        case _ => lv
      _ <- modify(_.update(l, refinedV))
    } yield ()

    /** refine types with type checks */
    def refineTypeCheck(
      ref: Ref,
      ty: ValueTy,
      positive: Boolean,
    )(using np: NodePoint[_]): Updater = for {
      l <- transfer(ref)
      v <- transfer(l)
      refinedV =
        if (positive)
          if (v.ty <= ty.toValue) v
          else v ⊓ AbsValue(ty)
        else v -- AbsValue(ty)
      _ <- modify(ref match
        case _: Local => _.update(l, refinedV)
        case Field(x: Local, EStr(field)) =>
          refineField(x, field, Binding(ty), positive)
        case _ => identity,
      )
      _ <- refine(v, refinedV)
    } yield ()
  }

  /** use type abstract domains */
  stateDomain = Some(StateTypeDomain(this))
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
      case (st, Param(x, ty, _, _)) => st.update(x, AbsValue(ty.ty))
    }

  /** logging the current analysis result */
  def logging: Unit = {
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
      silent = silent,
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
        name = "detailed type analysis result for each control point",
        data = sem.resultStrings(detail = true).mkString(LINE_SEP),
        filename = s"$ANALYZE_LOG_DIR/detailed-types",
        silent = silent,
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
    checkArity: Boolean = true,
    checkParamType: Boolean = true,
    checkReturnType: Boolean = true,
    checkUncheckedAbrupt: Boolean = false, // TODO
    checkInvalidBase: Boolean = false, // TODO
    checkUnaryOp: Boolean = true,
    checkBinaryOp: Boolean = true,
  )
