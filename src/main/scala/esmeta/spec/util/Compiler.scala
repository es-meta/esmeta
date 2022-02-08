package esmeta.spec.util

import esmeta.cfg.util.Builder
import esmeta.cfg.{Literal => CLiteral, Type => CType, *}
import esmeta.lang.*
import esmeta.spec.{Param => SParam, *}
import esmeta.util.BaseUtils.*

/** Compiler from metalangauge to CFG */
class Compiler(val spec: Spec) {

  /** get the compiled CFG */
  lazy val cfg: CFG = builder.cfg

  /** add an algorithm to a CFG as a function */
  def compile(algo: Algorithm): Func = {
    val head = algo.head
    val kind = getKind(head)
    val name = getName(head)
    val main = name == "RunJobs"
    val params = getParams(head)
    val fb = builder.FuncBuilder(main, kind, name, params)
    // TODO
    compile(fb, algo.body)
    fb.func
  }

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  // CFG builder
  private val builder = new Builder
  private type FB = builder.FuncBuilder

  // get function kind
  private def getKind(head: Head): Func.Kind = {
    import Func.Kind.*
    head match {
      case head: AbstractOperationHead       => AbsOp
      case head: NumericMethodHead           => NumMeth
      case head: SyntaxDirectedOperationHead => SynDirOp
      case head: ConcreteMethodHead          => ConcMeth
      case head: InternalMethodHead          => InternalMeth
      case head: BuiltinHead                 => Builtin
    }
  }

  // get function name
  private def getName(head: Head): String = {
    import Func.Kind.*
    head match {
      case head: AbstractOperationHead =>
        head.name
      case head: NumericMethodHead =>
        s"${head.ty}::${head.name}"
      case head: SyntaxDirectedOperationHead =>
        val Target = SyntaxDirectedOperationHead.Target
        val pre = head.target.fold("<DEFAULT>") {
          case Target(lhsName, idx, subIdx, _) => s"$lhsName[$idx,$subIdx]"
        }
        s"$pre.${head.methodName}"
      case head: ConcreteMethodHead =>
        s"${head.receiverParam.ty}.${head.methodName}"
      case head: InternalMethodHead =>
        s"${head.receiverParam.ty}.${head.methodName}"
      case head: BuiltinHead =>
        s"${head.ref}"
    }
  }

  // get function parameters
  private def getParams(head: Head): List[Func.Param] = {
    head match {
      case head: AbstractOperationHead =>
        head.params.map(compile)
      case head: NumericMethodHead =>
        head.params.map(compile)
      case head: SyntaxDirectedOperationHead =>
        PARAM_THIS :: head.withParams.map(compile)
      case head: ConcreteMethodHead =>
        compile(head.receiverParam) :: head.params.map(compile)
      case head: InternalMethodHead =>
        compile(head.receiverParam) :: head.params.map(compile)
      case head: BuiltinHead =>
        List(PARAM_THIS, PARAM_ARGS_LIST, PARAM_NEW_TARGET)
    }
  }

  // convert to references
  private inline def toStrERef(base: Ref, props: String*): ERef =
    ERef(toStrRef(base, props*))
  private inline def toStrRef(base: Ref, props: String*): Ref =
    toRef(base, props.map(EStr(_))*)
  private inline def toERef(base: Ref, props: Expr*): ERef =
    ERef(toRef(base, props*))
  private inline def toERef(fb: FB, base: Expr, props: Expr*): ERef =
    ERef(toRef(fb, base, props*))
  private inline def toRef(base: Ref, props: Expr*): Ref =
    props.foldLeft(base)(Prop(_, _))
  private inline def toRef(fb: FB, base: Expr, props: Expr*): Ref =
    toRef(getRef(fb, base), props*)
  private inline def getRef(fb: FB, expr: Expr): Ref = expr match {
    case ERef(ref) => ref
    case _         => val x = fb.newTId; fb.addInst(IAssign(x, expr)); x
  }

  // compile algorithm steps
  private def compile(fb: FB, step: Step): Unit = step match {
    case LetStep(x, expr) =>
      fb.addInst(ILet(compile(x), compile(fb, expr)))
    case SetStep(ref, expr) =>
      fb.addInst(IAssign(compile(fb, ref), compile(fb, expr)))
    case IfStep(cond, thenStep, elseStep) =>
      fb.addBranch(
        Branch.Kind.If,
        compile(fb, cond),
        compile(fb, thenStep),
        elseStep.map(compile(fb, _)),
      )
    case ReturnStep(expr) =>
      fb.addInst(IReturn(expr.fold(EUndef)(compile(fb, _))))
    case AssertStep(cond) =>
      fb.addInst(IAssert(compile(fb, cond)))
    case ForEachStep(ty, x, expr, body) =>
      val (i, iExpr) = fb.newTIdWithExpr
      val (length, lengthExpr) = fb.newTIdWithExpr
      val (list, listExpr) = fb.newTIdWithExpr
      fb.addInst(
        IAssign(list, compile(fb, expr)),
        IAssign(i, EMathVal(0)),
        IAssign(length, toStrERef(list, "length")),
      )
      fb.addBranch(
        Branch.Kind.Loop("foreach"),
        lessThan(iExpr, lengthExpr), {
          fb.addInst(ILet(compile(x), toERef(list, iExpr)))
          compile(fb, body)
          fb.addInst(IAssign(i, add(iExpr, EMathVal(1))))
        },
        loop = true,
      )
    case ForEachIntegerStep(x, start, cond, ascending, body) =>
      val (i, iExpr) = compileWithExpr(x)
      fb.addInst(ILet(i, compile(fb, start)))
      fb.addBranch(
        Branch.Kind.Loop("foreach-int"),
        compile(fb, cond), {
          compile(fb, body)
          val op = if (ascending) add(_, _) else sub(_, _)
          fb.addInst(IAssign(i, op(iExpr, EMathVal(1))))
        },
        loop = true,
      )
    case ThrowStep(errName) =>
      val expr = EMap(
        "OrdinaryObject",
        List(
          EStr("Prototype") -> compile(Intrinsic(errName, List("prototype"))),
          EStr("ErrorData") -> EUndef,
        ),
        fb.newSite,
      )
      val comp = EComp(ECONST_THROW, expr, ECONST_EMPTY)
      fb.addInst(IReturn(comp))
    case PerformStep(expr) =>
      fb.addInst(IExpr(compile(fb, expr)))
    case AppendStep(expr, ref) =>
      fb.addInst(IPush(compile(fb, expr), ERef(compile(fb, ref)), false))
    case RepeatStep(cond, body) =>
      fb.addBranch(
        Branch.Kind.Loop("repeat"),
        cond.fold(EBool(true))(compile(fb, _)),
        compile(fb, body),
        loop = true,
      )
    case PushCtxtStep(ref) =>
      fb.addInst(
        IAssign(GLOBAL_CONTEXT, ERef(compile(fb, ref))),
        IPush(EGLOBAL_CONTEXT, EGLOBAL_EXECUTION_STACK, false),
      )
    case NoteStep(note) =>
      fb.addInst(INop()) // XXX add edge to lang element
    case SuspendStep(context) =>
      fb.addInst(INop()) // XXX add edge to lang element
    case BlockStep(StepBlock(steps)) =>
      for (substep <- steps) compile(fb, substep.step)
    case YetStep(yet) =>
      fb.addInst(IExpr(EYet(yet.toString(false, false))))
  }

  // compile local variable
  private def compile(x: Variable): Name = Name(x.name)
  private def compileWithExpr(x: Variable): (Name, Expr) =
    val n = Name(x.name); (n, ERef(n))

  // compile references
  private def compile(fb: FB, ref: Reference): Ref = ref match {
    case x: Variable               => compile(x)
    case RunningExecutionContext() => GLOBAL_CONTEXT
    case CurrentRealmRecord()      => toStrRef(GLOBAL_CONTEXT, "Realm")
    case ActiveFunctionObject()    => toStrRef(GLOBAL_CONTEXT, "Function")
    case ref: PropertyReference    => compile(fb, ref)
  }

  private def compile(fb: FB, ref: PropertyReference): Prop =
    val PropertyReference(base, prop) = ref
    Prop(compile(fb, base), compile(fb, prop))

  // compile properties
  private def compile(fb: FB, prop: Property): Expr = prop match {
    case FieldProperty(field)    => compile(field)
    case ComponentProperty(name) => EStr(name)
    case IndexProperty(index)    => compile(fb, index)
  }

  // compile fields
  private def compile(field: Field): Expr = field match {
    case StringField(name)                      => EStr(name)
    case IntrinsicField(Intrinsic(base, props)) => ???
  }

  // compile expressions
  private def compile(fb: FB, expr: Expression): Expr = expr match {
    case StringConcatExpression(exprs) =>
      val es = exprs.map(compile(fb, _))
      es.reduce(add(_, _))
    case ListConcatExpression(exprs) => ???
    case RecordExpression(Type("Completion Record"), fields) =>
      val fmap = fields.toMap
      val fs @ List(ty, v, tgt) =
        List("Type", "Value", "Target").map(StringField(_))
      val keys = fmap.keySet
      if (keys != fs.toSet)
        error(s"invalid completion keys: ${keys.mkString(", ")}")
      EComp(
        compile(fb, fmap(ty)),
        compile(fb, fmap(v)),
        compile(fb, fmap(tgt)),
      )
    case RecordExpression(ty, fields) =>
      val props = fields.map { case (f, e) => compile(f) -> compile(fb, e) }
      EMap(ty.name, props, fb.newSite)
    case LengthExpression(ReferenceExpression(ref)) =>
      toStrERef(compile(fb, ref), "length")
    case LengthExpression(expr) =>
      val (x, xExpr) = fb.newTIdWithExpr
      fb.addInst(IAssign(x, compile(fb, expr)))
      toStrERef(x, "length")
    case SubstringExpression(expr, from, to) =>
      val (substr, substrExpr) = fb.newTIdWithExpr
      val (idx, idxExpr) = fb.newTIdWithExpr
      lazy val e = compile(fb, expr)
      lazy val f = compile(fb, from)
      lazy val t = compile(fb, to)
      fb.addInst(
        IAssign(substr, EStr("")),
        IAssign(idx, f),
      )
      fb.addBranch(
        Branch.Kind.Loop("substr"),
        lessThan(idxExpr, t),
        fb.addInst(
          IAssign(substr, add(substrExpr, toERef(fb, e, idxExpr))),
          IAssign(idx, add(idxExpr, one)),
        ),
        loop = true,
      )
      substrExpr
    case NumberOfExpression(ReferenceExpression(ref)) =>
      toStrERef(compile(fb, ref), "length")
    case NumberOfExpression(expr) =>
      val (x, xExpr) = fb.newTIdWithExpr
      fb.addInst(IAssign(x, compile(fb, expr)))
      toStrERef(x, "length")
    case IntrinsicExpression(intr)  => compile(intr)
    case SourceTextExpression(expr) => ???
    case InvokeAbstractOperationExpression(name, args) =>
      if (simpleOps contains name) simpleOps(name)(args.map(compile(fb, _)))
      else {
        val (x, xExpr) = fb.newTIdWithExpr
        val f = EClo(name, Nil)
        fb.addCall(x, f, args.map(compile(fb, _)))
        xExpr
      }
    case InvokeNumericMethodExpression(ty, name, args) =>
      val (x, xExpr) = fb.newTIdWithExpr
      val f = EClo(s"$ty::$name", Nil)
      fb.addCall(x, f, args.map(compile(fb, _)))
      xExpr
    case InvokeAbstractClosureExpression(ref, args) => ???
    case InvokeMethodExpression(ref, args) =>
      val (x, xExpr) = fb.newTIdWithExpr
      val prop @ Prop(base, _) = compile(fb, ref)
      fb.addCall(x, ERef(prop), ERef(base) :: args.map(compile(fb, _)))
      xExpr
    case InvokeSyntaxDirectedOperationExpression(base, name, args) => ???
    case ReturnIfAbruptExpression(expr, check) =>
      EReturnIfAbrupt(compile(fb, expr), check)
    case ListExpression(entries) =>
      EList(entries.map(compile(fb, _)), fb.newSite)
    case yet: YetExpression =>
      EYet(yet.toString(false, false))
    case ReferenceExpression(ref) =>
      ERef(compile(fb, ref))
    case MathOpExpression(op, args) =>
      import MathOpExpression.Op.*
      (op, args) match
        case (Max, _)           => EVariadic(VOp.Max, args.map(compile(fb, _)))
        case (Min, _)           => EVariadic(VOp.Min, args.map(compile(fb, _)))
        case (Abs, List(arg))   => EUnary(UOp.Abs, compile(fb, arg))
        case (Floor, List(arg)) => EUnary(UOp.Floor, compile(fb, arg))
        case (ToBigInt, List(arg)) => EConvert(COp.ToBigInt, compile(fb, arg))
        case (ToNumber, List(arg)) => EConvert(COp.ToNumber, compile(fb, arg))
        case (ToMath, List(arg))   => EConvert(COp.ToMath, compile(fb, arg))
        case _                     => error(s"invalid math operation: $expr")
    case ExponentiationExpression(base, power) =>
      EBinary(BOp.Pow, compile(fb, base), compile(fb, power))
    case BinaryExpression(left, op, right) =>
      EBinary(compile(op), compile(fb, left), compile(fb, right))
    case UnaryExpression(op, expr) =>
      EUnary(compile(op), compile(fb, expr))
    case AbstractClosureExpression(params, captured, body) =>
      val ps =
        params.map(x => Func.Param(compile(x), false, CType("any")))
      val newFb = builder.FuncBuilder(false, Func.Kind.Clo, fb.name, ps)
      compile(newFb, body)
      EClo(newFb.func.name, captured.map(compile))
    case lit: Literal => compile(lit)
  }

  // compile binary operators
  private def compile(op: BinaryExpression.Op): BOp = op match
    case BinaryExpression.Op.Add => BOp.Plus
    case BinaryExpression.Op.Sub => BOp.Sub
    case BinaryExpression.Op.Mul => BOp.Mul
    case BinaryExpression.Op.Div => BOp.Div
    case BinaryExpression.Op.Mod => BOp.Mod

  // compile unary operators
  private def compile(op: UnaryExpression.Op): UOp = op match
    case UnaryExpression.Op.Neg => UOp.Neg

  // compile literals
  private def compile(lit: Literal): Expr = lit match {
    case ThisLiteral()                      => ENAME_THIS
    case NewTargetLiteral()                 => ENAME_NEW_TARGET
    case HexLiteral(hex, name)              => EMathVal(hex)
    case CodeLiteral(code)                  => EStr(code)
    case NonterminalLiteral(ordinal, name)  => ??? // TODO field of *this*
    case ConstLiteral(name)                 => EConst(name)
    case StringLiteral(s)                   => EStr(s)
    case FieldLiteral(field)                => ???
    case SymbolLiteral(sym)                 => toERef(GLOBAL_SYMBOL, EStr(sym))
    case PositiveInfinityMathValueLiteral() => ENumber(Double.PositiveInfinity)
    case NegativeInfinityMathValueLiteral() => ENumber(Double.NegativeInfinity)
    case DecimalMathValueLiteral(n)         => EMathVal(n)
    case NumberLiteral(n)                   => ENumber(n)
    case BigIntLiteral(n)                   => EBigInt(n)
    case TrueLiteral()                      => EBool(true)
    case FalseLiteral()                     => EBool(false)
    case UndefinedLiteral()                 => EUndef
    case NullLiteral()                      => ENull
    case AbsentLiteral()                    => EAbsent
    case UndefinedTypeLiteral()             => EGLOBAL_UNDEF_TYPE
    case NullTypeLiteral()                  => EGLOBAL_NULL_TYPE
    case BooleanTypeLiteral()               => EGLOBAL_BOOL_TYPE
    case StringTypeLiteral()                => EGLOBAL_STRING_TYPE
    case SymbolTypeLiteral()                => EGLOBAL_SYMBOL_TYPE
    case NumberTypeLiteral()                => EGLOBAL_NUMBER_TYPE
    case BigIntTypeLiteral()                => EGLOBAL_BIGINT_TYPE
    case ObjectTypeLiteral()                => EGLOBAL_OBJECT_TYPE
  }

  // compile branch conditions
  private def compile(fb: FB, cond: Condition): Expr = cond match {
    case ExpressionCondition(expr) =>
      compile(fb, expr)
    case InstanceOfCondition(expr, neg, tys) =>
      val (x, xExpr) = fb.newTIdWithExpr
      fb.addInst(IAssign(x, compile(fb, expr)))
      val e = tys.map[Expr](t => ETypeCheck(xExpr, compile(t))).reduce(or(_, _))
      if (neg) not(e) else e
    case HasFieldCondition(ref, neg, field) =>
      val e = exist(toERef(compile(fb, ref), compile(field)))
      if (neg) not(e) else e
    case AbruptCompletionCondition(x, negation) => ???
    case IsAreCondition(left, neg, right) =>
      val es = for (lexpr <- left) yield {
        val l = compile(fb, lexpr)
        val e = right
          .map(r => EBinary(BOp.Eq, l, compile(fb, r)))
          .reduce(or(_, _))
        if (neg) not(e) else e
      }
      es.reduce(and(_, _))
    case BinaryCondition(left, op, right) =>
      import BinaryCondition.Op.*
      lazy val l = compile(fb, left)
      lazy val r = compile(fb, right)
      op match {
        case Eq               => EBinary(BOp.Equal, l, r)
        case NEq              => not(EBinary(BOp.Equal, l, r))
        case LessThan         => lessThan(l, r)
        case LessThanEqual    => not(lessThan(r, l))
        case GreaterThan      => lessThan(r, l)
        case GreaterThanEqual => not(lessThan(l, r))
        case SameCodeUnits    => EBinary(BOp.Eq, l, r)
        case Contains         => EContains(l, r)
        case NContains        => not(EContains(l, r))
      }
    case CompoundCondition(left, op, right) =>
      lazy val l = compile(fb, left)
      lazy val r = compile(fb, right)
      op match
        case CompoundCondition.Op.And   => and(l, r)
        case CompoundCondition.Op.Or    => or(l, r)
        case CompoundCondition.Op.Imply => or(not(l), r)
  }

  // compile algorithm parameters
  private def compile(param: SParam): Func.Param = {
    val SParam(name, skind, ty) = param
    val optional = skind == SParam.Kind.Optional
    Func.Param(Name(name), optional, CType(ty))
  }

  // compile types
  private def compile(ty: Type): CType = CType(ty.name)

  // compile intrinsics
  private def compile(intrinsic: Intrinsic): Expr =
    toERef(GLOBAL_INTRINSIC, EStr(intrinsic.toString))

  // literal helpers
  private val zero = EMathVal(0)
  private val one = EMathVal(1)

  // operation helpers
  private inline def exist(expr: Expr) = EBinary(BOp.Eq, expr, EAbsent)
  private inline def not(expr: Expr) = EUnary(UOp.Not, expr)
  private inline def lessThan(l: Expr, r: Expr) = EBinary(BOp.Lt, l, r)
  private inline def add(l: Expr, r: Expr) = EBinary(BOp.Plus, l, r)
  private inline def sub(l: Expr, r: Expr) = EBinary(BOp.Sub, l, r)
  private inline def and(l: Expr, r: Expr) = EBinary(BOp.And, l, r)
  private inline def or(l: Expr, r: Expr) = EBinary(BOp.Or, l, r)

  // simple operations
  type SimpleOp = PartialFunction[List[Expr], Expr]
  def arityCheck(pair: (String, SimpleOp)): (String, SimpleOp) = {
    val (name, f) = pair
    name -> (args =>
      optional(f(args)).getOrElse(
        error(s"invalid arguments: $name(${args.mkString(", ")})"),
      ),
    )
  }
  val simpleOps: Map[String, SimpleOp] = Map(
    arityCheck("ParseText" -> { case List(code, rule) => EParse(code, rule) }),
    arityCheck("Type" -> { case List(expr) => ETypeOf(expr) }),
    // arityCheck("GetArgument" -> ???),
    // arityCheck("IsDuplicate" -> ???),
    // arityCheck("IsArrayIndex" -> ???),
    // arityCheck("ThrowCompletion" -> ???),
    // arityCheck("NormalCompletion" -> ???),
    // arityCheck("IsAbruptCompletion" -> ???),
  )
}
object Compiler {

  /** compile a specification to a CFG */
  def apply(spec: Spec): CFG = {
    val compiler = new Compiler(spec)
    for (algo <- spec.algorithms) compiler.compile(algo)
    compiler.cfg
  }

  /** compile an algorithm to a function */
  def apply(spec: Spec, algo: Algorithm): Func = {
    val compiler = new Compiler(spec)
    compiler.compile(algo)
  }
}
