package esmeta.cfg

import esmeta.spec.{Param => SParam, *}
import esmeta.lang.{Block => LBlock, *}

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
  private def getParams(head: Head): List[Param] = {
    import Func.Kind.*
    import Param.Kind.*
    head match {
      case head: AbstractOperationHead =>
        head.params.map(compile)
      case head: NumericMethodHead =>
        head.params.map(compile)
      case head: SyntaxDirectedOperationHead => ???
      case head: ConcreteMethodHead          => ???
      case head: InternalMethodHead          => ???
      case head: BuiltinHead                 => ???
    }
  }

  // convert to references
  private def toStrERef(base: Ref, props: String*): ERef =
    ERef(toStrRef(base, props*))
  private def toStrRef(base: Ref, props: String*): Ref =
    toRef(base, props.map(EStr(_))*)
  private def toERef(base: Ref, props: Expr*): ERef =
    ERef(toRef(base, props*))
  private def toRef(base: Ref, props: Expr*): Ref =
    props.foldLeft(base)(Prop(_, _))

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
      val i = fb.newTId; val iExpr = ERef(i)
      val length = fb.newTId; val lengthExpr = ERef(length)
      val list = fb.newTId; val listExpr = ERef(list)
      fb.addInst(
        IAssign(list, compile(fb, expr)),
        IAssign(i, EMathVal(0)),
        IAssign(length, toStrERef(list, "length")),
      )
      fb.addBranch(
        Branch.Kind.Foreach,
        EBinary(BOp.Lt, iExpr, lengthExpr), {
          fb.addInst(ILet(compile(x), toERef(list, iExpr)))
          compile(fb, body)
          fb.addInst(IAssign(i, EBinary(BOp.Plus, iExpr, EMathVal(1))))
        },
        { /* do nothing */ },
      )
    case ForEachIntegerStep(x, start, cond, ascending, body) =>
      val i = compile(x); val iExpr = ERef(i)
      fb.addInst(ILet(i, compile(fb, start)))
      fb.addBranch(
        Branch.Kind.Foreach,
        compile(fb, cond), {
          compile(fb, body)
          val bop = if (ascending) BOp.Plus else BOp.Sub
          fb.addInst(IAssign(i, EBinary(bop, iExpr, EMathVal(1))))
        },
        { /* do nothing */ },
      )
    case ThrowStep(errorName) =>
      val expr = EMap(
        "OrdinaryObject",
        List(
          EStr("Prototype") -> compile(Intrinsic(errorName, List("prototype"))),
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
        Branch.Kind.Repeat,
        cond.fold(EBool(true))(compile(fb, _)),
        compile(fb, body),
        { /* do nothing */ },
      )
    case PushCtxtStep(ref) =>
      fb.addInst(
        IAssign(GLOBAL_CONTEXT, ERef(compile(fb, ref))),
        IPush(EGLOBAL_CONTEXT, EGLOBAL_EXECUTION_STACK, false),
      )
    case NoteStep(note) => /* do nothing */
    case SuspendStep(context) =>
      ???
    case BlockStep(StepBlock(steps)) =>
      for (substep <- steps) compile(fb, substep.step)
    case YetStep(yet) =>
      fb.addInst(IExpr(EYet(yet.toString(false, false))))
  }

  // compile local variable
  private def compile(x: Variable): Name = Name(x.name)

  // compile references
  private def compile(fb: FB, ref: Reference): Ref = ref match {
    case x: Variable               => compile(x)
    case RunningExecutionContext() => GLOBAL_CONTEXT
    case CurrentRealmRecord()      => GLOBAL_REALM
    case ActiveFunctionObject()    => toStrRef(GLOBAL_CONTEXT, "Function")
    case PropertyReference(base, prop) =>
      Prop(compile(fb, ref), compile(fb, prop))
  }

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
    case ExprBlock(exprs)                                          => ???
    case LetStep(variable, expr)                                   => ???
    case SetStep(ref, expr)                                        => ???
    case ReturnStep(expr)                                          => ???
    case PerformStep(expr)                                         => ???
    case AppendStep(elem, ref)                                     => ???
    case YetStep(expr)                                             => ???
    case StringConcatExpression(exprs)                             => ???
    case ListConcatExpression(exprs)                               => ???
    case RecordExpression(ty, fields)                              => ???
    case LengthExpression(expr)                                    => ???
    case SubstringExpression(expr, from, to)                       => ???
    case NumberOfExpression(expr)                                  => ???
    case IntrinsicExpression(Intrinsic(base, props))               => ???
    case SourceTextExpression(expr)                                => ???
    case InvokeAbstractOperationExpression(name, args)             => ???
    case InvokeNumericMethodExpression(ty, name, args)             => ???
    case InvokeAbstractClosureExpression(ref, args)                => ???
    case InvokeMethodExpression(ref, args)                         => ???
    case InvokeSyntaxDirectedOperationExpression(base, name, args) => ???
    case ReturnIfAbruptExpression(expr, check)                     => ???
    case ListExpression(entries)                                   => ???
    case YetExpression(str, block)                                 => ???
    case ReferenceExpression(ref)                                  => ???
    case MathOpExpression(op, args)                                => ???
    case ExponentiationExpression(base, power)                     => ???
    case BinaryExpression(left, op, right)                         => ???
    case UnaryExpression(op, expr)                                 => ???
    case AbstractClosureExpression(params, captured, body)         => ???
    case lit: Literal => compile(lit)
  }

  private def compile(lit: Literal): Expr = lit match {
    case ThisLiteral()                      => ???
    case NewTargetLiteral()                 => ???
    case HexLiteral(hex, name)              => ???
    case CodeLiteral(code)                  => ???
    case NonterminalLiteral(ordinal, name)  => ???
    case ConstLiteral(name)                 => ???
    case StringLiteral(s)                   => ???
    case FieldLiteral(field)                => ???
    case SymbolLiteral(sym)                 => ???
    case PositiveInfinityMathValueLiteral() => ???
    case NegativeInfinityMathValueLiteral() => ???
    case DecimalMathValueLiteral(n)         => ???
    case NumberLiteral(n)                   => ???
    case BigIntLiteral(n)                   => ???
    case TrueLiteral()                      => ???
    case FalseLiteral()                     => ???
    case UndefinedLiteral()                 => ???
    case NullLiteral()                      => ???
    case AbsentLiteral()                    => ???
    case UndefinedTypeLiteral()             => ???
    case NullTypeLiteral()                  => ???
    case BooleanTypeLiteral()               => ???
    case StringTypeLiteral()                => ???
    case SymbolTypeLiteral()                => ???
    case NumberTypeLiteral()                => ???
    case BigIntTypeLiteral()                => ???
    case ObjectTypeLiteral()                => ???
  }

  // compile branch conditions
  private def compile(fb: FB, cond: Condition): Expr = cond match {
    case ExpressionCondition(expr)                => ???
    case InstanceOfCondition(expr, negation, ty)  => ???
    case HasFieldCondition(expr, negation, field) => ???
    case AbruptCompletionCondition(x, negation)   => ???
    case IsAreCondition(left, negation, right)    => ???
    case BinaryCondition(left, op, right)         => ???
    case CompoundCondition(left, op, right)       => ???
  }

  // compile algorithm parameters
  private def compile(param: SParam): Param = {
    val SParam(name, kind, ty) = param
    ???
  }

  // compile intrinsics
  private def compile(intrinsic: Intrinsic): Expr = ???
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
