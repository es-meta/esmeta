package esmeta.cfg

import esmeta.spec.{Param => SParam, *}
import esmeta.lang.{Block => LBlock, Type => LType, *}

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
          fb.addInst(IAssign(i, EBinary(BOp.Plus, iExpr, EMathVal(1))))
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
          val bop = if (ascending) BOp.Plus else BOp.Sub
          fb.addInst(IAssign(i, EBinary(bop, iExpr, EMathVal(1))))
        },
        loop = true,
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
  private def compileWithExpr(x: Variable): (Name, Expr) =
    val n = Name(x.name); (n, ERef(n))

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
    case StringConcatExpression(exprs) =>
      val es = exprs.map(compile(fb, _))
      es.reduce(EBinary(BOp.Plus, _, _))
    case ListConcatExpression(exprs)  => ???
    case RecordExpression(ty, fields) => ???
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
    case IntrinsicExpression(Intrinsic(base, props)) => ???
    case SourceTextExpression(expr)                  => ???
    case InvokeAbstractOperationExpression(name, args) =>
      val (x, xExpr) = fb.newTIdWithExpr
      val f = EClo(name, Nil)
      fb.addCall(x, f, args.map(compile(fb, _)))
      xExpr
    case InvokeNumericMethodExpression(ty, name, args)             => ???
    case InvokeAbstractClosureExpression(ref, args)                => ???
    case InvokeMethodExpression(ref, args)                         => ???
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
      println((op, args))
      ???
    case ExponentiationExpression(base, power) =>
      EBinary(BOp.Pow, compile(fb, base), compile(fb, power))
    case BinaryExpression(left, op, right) =>
      EBinary(compile(op), compile(fb, left), compile(fb, right))
    case UnaryExpression(op, expr) =>
      EUnary(compile(op), compile(fb, expr))
    case AbstractClosureExpression(params, captured, body) =>
      val ps =
        params.map(x => Param(compile(x), Param.Kind.Normal, Type("any")))
      val newFb = builder.FuncBuilder(false, Func.Kind.Clo, fb.name, ps)
      compile(newFb, body)
      EClo(newFb.func.name, captured.map(compile))
    case lit: Literal => compile(lit)
  }

  // compile binary operators
  private def compile(op: BinaryExpression.Op): BOp = op match {
    case BinaryExpression.Op.Add => BOp.Plus
    case BinaryExpression.Op.Sub => BOp.Sub
    case BinaryExpression.Op.Mul => BOp.Mul
    case BinaryExpression.Op.Div => BOp.Div
    case BinaryExpression.Op.Mod => BOp.Mod
  }

  // compile unary operators
  private def compile(op: UnaryExpression.Op): UOp = op match {
    case _ => ???
  }

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
    case SymbolLiteral(sym)                 => ??? // TODO Refer Table 1
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
    case UndefinedTypeLiteral()             => ??? // TODO type literals
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
    case ExpressionCondition(expr) =>
      compile(fb, expr)
    case InstanceOfCondition(expr, negation, ty)  => ???
    case HasFieldCondition(expr, negation, field) => ???
    case AbruptCompletionCondition(x, negation)   => ???
    case IsAreCondition(left, neg, right) =>
      val es = for (lexpr <- left) yield {
        val l = compile(fb, lexpr)
        val e = right
          .map(r => EBinary(BOp.Eq, l, compile(fb, r)))
          .reduce(EBinary(BOp.Or, _, _))
        if (neg) not(e) else e
      }
      es.reduce(EBinary(BOp.And, _, _))
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
      EBinary(compile(op), compile(fb, left), compile(fb, right))
  }

  // compile compound condition operators
  private def compile(op: CompoundCondition.Op): BOp = op match {
    case CompoundCondition.Op.And   => BOp.And
    case CompoundCondition.Op.Or    => BOp.Or
    case CompoundCondition.Op.Imply => ???
  }

  // compile algorithm parameters
  private def compile(param: SParam): Param = {
    val SParam(name, skind, ty) = param
    val kind = skind match {
      case SParam.Kind.Normal   => Param.Kind.Normal
      case SParam.Kind.Optional => Param.Kind.Optional
      case _                    => ???
    }
    Param(Name(name), kind, Type(ty))
  }

  // compile intrinsics
  private def compile(intrinsic: Intrinsic): Expr = ???

  // literal helpers
  private val zero = EMathVal(0)
  private val one = EMathVal(1)

  // operation helpers
  private inline def not(expr: Expr) = EUnary(UOp.Not, expr)
  private inline def lessThan(l: Expr, r: Expr) = EBinary(BOp.Lt, l, r)
  private inline def add(l: Expr, r: Expr) = EBinary(BOp.Plus, l, r)
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
