package esmeta.spec.util

import esmeta.ir.{Type => IRType, *}
import esmeta.ir.util.{Parser => IRParser}
import esmeta.lang.*
import esmeta.spec.{Param => SParam, *}
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import scala.collection.mutable.ListBuffer
import esmeta.MANUALS_DIR

/** Compiler from metalangauge to IR */
class Compiler(val spec: Spec) {

  /** compiled specification */
  def result: Program = {
    // compile algorithms in spec
    for (algo <- spec.algorithms) compile(algo)

    // load manually created AOs
    for (file <- walkTree(MANUALS_DIR) if irFilter(file.getName))
      funcs += Func.fromFile(file.toString)

    // result
    val program = Program(funcs.toList)

    // connect backward edge to a given specification
    program.spec = spec

    program
  }

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  private val manualRules: Map[String, Inst] = (for {
    (yet, inst) <- readJson[Map[String, String]](s"$MANUALS_DIR/rule.json")
  } yield yet -> Inst.from(inst)).toMap

  // compiled algorithms
  private val funcs: ListBuffer[Func] = ListBuffer()

  // TODO refactoring
  // function builder
  private case class FuncBuilder(
    kind: Func.Kind,
    name: String,
    params: List[Func.Param],
    body: Step,
    algo: Algorithm,
  ) {
    // contexts
    private var contexts: List[ListBuffer[Inst]] = List()
    def newContext: Unit = contexts = ListBuffer() :: contexts
    def popContext: Inst = contexts match
      case current :: rest => contexts = rest; ISeq(current.toList)
      case _               => ??? // error
    def addInst(insts: Inst*): Unit = contexts match
      case current :: rest => current ++= insts
      case _               => ??? // error

    // temporal identifier id counter
    private def nextTId: Int = { val tid = tidCount; tidCount += 1; tid }
    private var tidCount: Int = 0

    /** get next temporal identifier */
    def newTId: Temp = Temp(nextTId)
    def newTIdWithExpr: (Temp, Expr) = { val x = newTId; (x, ERef(x)) }

    /** get a compiled algoirhtm */
    lazy val result: Func = {
      val inst = compileWithContext(this, body)
      val func = Func(false, kind, name, params, inst, Some(algo))
      funcs += func
      func
    }
  }
  private type FB = FuncBuilder

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
  private inline def toIntrinsic(base: Ref, intr: Intrinsic): Prop =
    // XXX consider external property access (SubMap)
    val intrBase = Prop(base, EStr(intr.base))
    val path = intr.props.flatMap(p => List("SubMap", p, "Value").map(EStr(_)))
    path.foldLeft(intrBase) { case (b, p) => Prop(b, p) }
  private inline def toEIntrinsic(base: Ref, intr: Intrinsic): ERef =
    toERef(toIntrinsic(base, intr))
  private inline def currentRealm: Ref = toStrRef(GLOBAL_CONTEXT, "Realm")
  private inline def currentIntrinsics: Ref =
    toStrRef(currentRealm, "Intrinsics")

  // compile with new context and pop instructions
  private def compileWithContext(fb: FB, step: Step): Inst =
    compileWithContext(fb, compile(fb, step))
  private def compileWithContext(fb: FB, f: => Unit): Inst =
    fb.newContext; f; fb.popContext

  /** compile an algorithm to an IR function */
  private def compile(algo: Algorithm): Func = FuncBuilder(
    getKind(algo.head),
    getName(algo.head),
    getParams(algo.head),
    algo.body,
    algo,
  ).result

  // compile algorithm steps
  private def compile(fb: FB, step: Step): Unit = step match {
    case LetStep(x, expr) =>
      fb.addInst(ILet(compile(x), compile(fb, expr)))
    case SetStep(ref, expr) =>
      fb.addInst(IAssign(compile(fb, ref), compile(fb, expr)))
    case IfStep(cond, thenStep, elseStep) =>
      fb.addInst(
        IIf(
          compile(fb, cond),
          compileWithContext(fb, thenStep),
          elseStep.fold(ISeq(List()))(compileWithContext(fb, _)),
        ),
      )
    case ReturnStep(expr) =>
      fb.addInst(IReturn(expr.fold(EUndef)(compile(fb, _))))
    case AssertStep(cond) =>
      fb.addInst(IAssert(compile(fb, cond)))
    case ForEachStep(ty, x, expr, true, body) =>
      val (i, iExpr) = fb.newTIdWithExpr
      val (length, lengthExpr) = fb.newTIdWithExpr
      val (list, listExpr) = fb.newTIdWithExpr
      fb.addInst(
        IAssign(list, compile(fb, expr)),
        IAssign(i, EMathVal(0)),
        IAssign(length, toStrERef(list, "length")),
      )
      fb.addInst(
        ILoop(
          "foreach",
          lessThan(iExpr, lengthExpr),
          compileWithContext(
            fb, {
              fb.addInst(ILet(compile(x), toERef(list, iExpr)))
              compile(fb, body)
              fb.addInst(IAssign(i, add(iExpr, EMathVal(1))))
            },
          ),
        ),
      )
    case ForEachStep(ty, x, expr, false, body) =>
      val (i, iExpr) = fb.newTIdWithExpr
      val (list, listExpr) = fb.newTIdWithExpr
      fb.addInst(
        IAssign(list, compile(fb, expr)),
        IAssign(i, toStrERef(list, "length")),
      )
      fb.addInst(
        ILoop(
          "foreach",
          lessThan(EMathVal(0), iExpr),
          compileWithContext(
            fb, {
              fb.addInst(IAssign(i, sub(iExpr, EMathVal(1))))
              fb.addInst(ILet(compile(x), toERef(list, iExpr)))
              compile(fb, body)
            },
          ),
        ),
      )
    case ForEachIntegerStep(x, start, cond, ascending, body) =>
      val (i, iExpr) = compileWithExpr(x)
      fb.addInst(ILet(i, compile(fb, start)))
      fb.addInst(
        ILoop(
          "foreach-int",
          compile(fb, cond),
          compileWithContext(
            fb, {
              compile(fb, body)
              val op = if (ascending) add(_, _) else sub(_, _)
              fb.addInst(IAssign(i, op(iExpr, EMathVal(1))))
            },
          ),
        ),
      )
    case ThrowStep(errName) =>
      val proto = Intrinsic(errName, List("prototype"))
      val expr = EMap(
        "OrdinaryObject",
        List(
          EStr("Prototype") -> toEIntrinsic(currentIntrinsics, proto),
          EStr("ErrorData") -> EUndef,
        ),
      )
      val comp = EComp(ECONST_THROW, expr, ECONST_EMPTY)
      fb.addInst(IReturn(comp))
    case PerformStep(expr) =>
      fb.addInst(IExpr(compile(fb, expr)))
    case AppendStep(expr, ref) =>
      fb.addInst(IPush(compile(fb, expr), ERef(compile(fb, ref)), false))
    case RepeatStep(cond, body) =>
      fb.addInst(
        ILoop(
          "repeat",
          cond.fold(EBool(true))(compile(fb, _)),
          compileWithContext(fb, body),
        ),
      )
    case PushCtxtStep(ref) =>
      fb.addInst(
        IAssign(GLOBAL_CONTEXT, ERef(compile(fb, ref))),
      )
      fb.addInst(IPush(EGLOBAL_CONTEXT, EGLOBAL_EXECUTION_STACK, false))
    case NoteStep(note) =>
      fb.addInst(INop()) // XXX add edge to lang element
    case SuspendStep(context, false) =>
      fb.addInst(INop()) // XXX add edge to lang element
    case SuspendStep(context, true) =>
      fb.addInst(IExpr(EPop(EGLOBAL_EXECUTION_STACK, false)))
    case BlockStep(StepBlock(steps)) =>
      for (substep <- steps) compile(fb, substep.step)
    case YetStep(yet) =>
      val yetStr = yet.toString(true, false)
      fb.addInst(
        if (manualRules contains yetStr) manualRules(yetStr)
        else IExpr(EYet(yetStr)),
      )
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
    val baseRef = compile(fb, base)
    prop match
      case FieldProperty(name)     => Prop(baseRef, EStr(name))
      case ComponentProperty(name) => Prop(baseRef, EStr(name))
      case IndexProperty(index)    => Prop(baseRef, compile(fb, index))
      case IntrinsicProperty(intr) => toIntrinsic(baseRef, intr)

  // compile expressions
  private def compile(fb: FB, expr: Expression): Expr = expr match {
    case StringConcatExpression(exprs) =>
      EStrConcat(exprs.map(compile(fb, _)))
    case ListConcatExpression(exprs) =>
      EListConcat(exprs.map(compile(fb, _)))
    case RecordExpression(Type("Completion Record"), fields) =>
      val fmap = fields.toMap
      val fs @ List(ty, v, tgt) =
        List("Type", "Value", "Target").map(FieldLiteral(_))
      val keys = fmap.keySet
      if (keys != fs.toSet)
        error(s"invalid completion keys: ${keys.mkString(", ")}")
      EComp(
        compile(fb, fmap(ty)),
        compile(fb, fmap(v)),
        compile(fb, fmap(tgt)),
      )
    case RecordExpression(ty, fields) =>
      val props = fields.map { case (f, e) => compile(fb, f) -> compile(fb, e) }
      EMap(ty.name, props)
    case LengthExpression(ReferenceExpression(ref)) =>
      toStrERef(compile(fb, ref), "length")
    case LengthExpression(expr) =>
      val (x, xExpr) = fb.newTIdWithExpr
      fb.addInst(IAssign(x, compile(fb, expr)))
      toStrERef(x, "length")
    case SubstringExpression(expr, from, to) =>
      ESubstring(
        compile(fb, expr),
        compile(fb, from),
        compile(fb, to),
      )
    case NumberOfExpression(ReferenceExpression(ref)) =>
      toStrERef(compile(fb, ref), "length")
    case NumberOfExpression(expr) =>
      val (x, xExpr) = fb.newTIdWithExpr
      fb.addInst(IAssign(x, compile(fb, expr)))
      toStrERef(x, "length")
    case IntrinsicExpression(intr) =>
      toEIntrinsic(currentIntrinsics, intr)
    case SourceTextExpression(expr) =>
      ESourceText(compile(fb, expr))
    case CoveredByExpression(code, rule) =>
      EParse(compile(fb, code), compile(fb, rule))
    case InvokeAbstractOperationExpression(name, args) =>
      if (simpleOps contains name) simpleOps(name)(args.map(compile(fb, _)))
      else {
        val (x, xExpr) = fb.newTIdWithExpr
        val f = EClo(name, Nil)
        fb.addInst(ICall(x, f, args.map(compile(fb, _))))
        xExpr
      }
    case InvokeNumericMethodExpression(ty, name, args) =>
      val (x, xExpr) = fb.newTIdWithExpr
      val f = EClo(s"$ty::$name", Nil)
      fb.addInst(ICall(x, f, args.map(compile(fb, _))))
      xExpr
    case InvokeAbstractClosureExpression(ref, args) =>
      val (x, xExpr) = fb.newTIdWithExpr
      fb.addInst(ICall(x, ERef(compile(fb, ref)), args.map(compile(fb, _))))
      xExpr
    case InvokeMethodExpression(ref, args) =>
      val (x, xExpr) = fb.newTIdWithExpr
      val prop @ Prop(base, _) = compile(fb, ref)
      fb.addInst(ICall(x, ERef(prop), ERef(base) :: args.map(compile(fb, _))))
      xExpr
    case InvokeSyntaxDirectedOperationExpression(base, name, args) =>
      val baseExpr = compile(fb, base)
      val callRef = toERef(fb, baseExpr, EStr(name))
      base match
        case NonterminalLiteral(_, ntName)
            if spec.grammar.topLevelLexicals.contains(ntName) =>
          callRef
        case _ =>
          val (x, xExpr) = fb.newTIdWithExpr
          fb.addInst(ICall(x, callRef, baseExpr :: args.map(compile(fb, _))))
          xExpr
    case ReturnIfAbruptExpression(expr, check) =>
      EReturnIfAbrupt(compile(fb, expr), check)
    case ListExpression(entries) =>
      EList(entries.map(compile(fb, _)))
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
        params.map(x => Func.Param(compile(x), false, IRType("any")))
      val cloName = s"${fb.name}:clo"
      val newFb = FuncBuilder(Func.Kind.Clo, cloName, ps, body, fb.algo)
      newFb.result
      EClo(cloName, captured.map(compile))
    case lit: Literal => compile(fb, lit)
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
  private def compile(fb: FB, lit: Literal): Expr = lit match {
    case ThisLiteral()         => ENAME_THIS
    case NewTargetLiteral()    => ENAME_NEW_TARGET
    case HexLiteral(hex, name) => EMathVal(hex)
    case CodeLiteral(code)     => EStr(code)
    case NonterminalLiteral(ordinal, name) =>
      fb.algo.head match
        case SyntaxDirectedOperationHead(Some(target), method, _, _) =>
          val rhsNames = target.rhsParams.map(_.name)
          // check if it is used as reference
          // TODO ClassTail[0,3].Contains
          if (rhsNames contains name) {
            val xs = rhsNames.zipWithIndex.filter(_._1 == name)
            val idx = xs(ordinal.getOrElse(1) - 1)._2
            toERef(NAME_THIS, EMathVal(idx))
          } else EGrammar(name, Nil) // TODO grammar params
        case _ => EGrammar(name, Nil) // TODO grammar params
    case ConstLiteral(name)                 => EConst(name)
    case StringLiteral(s)                   => EStr(s)
    case FieldLiteral(field)                => EStr(field)
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
      val e = isAbsent(toERef(compile(fb, ref), compile(fb, field)))
      if (neg) e else not(e)
    case AbruptCompletionCondition(x, neg) =>
      val ref = toRef(compile(x))
      val abrupt = not(
        EBinary(BOp.Eq, toERef(ref, EStr("Type")), ECONST_NORMAL),
      )
      and(EIsCompletion(ERef(ref)), abrupt)
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
    Func.Param(Name(name), optional, IRType(ty))
  }

  // compile types
  private def compile(ty: Type): IRType = IRType(ty.name)

  // literal helpers
  private val zero = EMathVal(0)
  private val one = EMathVal(1)

  // operation helpers
  private inline def isAbsent(expr: Expr) = EBinary(BOp.Eq, expr, EAbsent)
  private inline def not(expr: Expr) = EUnary(UOp.Not, expr)
  private inline def lessThan(l: Expr, r: Expr) = EBinary(BOp.Lt, l, r)
  private inline def add(l: Expr, r: Expr) = EBinary(BOp.Plus, l, r)
  private inline def sub(l: Expr, r: Expr) = EBinary(BOp.Sub, l, r)
  private inline def and(l: Expr, r: Expr) = EBinary(BOp.And, l, r)
  private inline def or(l: Expr, r: Expr) = EBinary(BOp.Or, l, r)

  // simple operations
  private type SimpleOp = PartialFunction[List[Expr], Expr]
  private def arityCheck(pair: (String, SimpleOp)): (String, SimpleOp) = {
    val (name, f) = pair
    name -> (args =>
      optional(f(args)).getOrElse(
        error(s"invalid arguments: $name(${args.mkString(", ")})"),
      ),
    )
  }
  private val simpleOps: Map[String, SimpleOp] = Map(
    arityCheck("ParseText" -> { case List(code, rule) => EParse(code, rule) }),
    arityCheck("Type" -> { case List(expr) => ETypeOf(expr) }),
    arityCheck("Completion" -> { case List(expr) => expr }), // TODO needed?
    // arityCheck("GetArgument" -> ???),
    // arityCheck("IsDuplicate" -> ???),
    // arityCheck("IsArrayIndex" -> ???),
    // arityCheck("ThrowCompletion" -> ???),
    // arityCheck("NormalCompletion" -> ???),
    // arityCheck("IsAbruptCompletion" -> ???),
  )
}
object Compiler {
  def apply(spec: Spec): Program = new Compiler(spec).result
}
