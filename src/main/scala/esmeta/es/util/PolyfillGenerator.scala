package esmeta.es.util

import esmeta.es.*
import esmeta.lang.*
import esmeta.spec.*
import esmeta.util.BaseUtils.*
import esmeta.lang.util.{UnitWalker => LangUnitWalker}

/** polyfill generator */
object PolyfillGenerator {
  def apply(spec: Spec): List[Polyfill] = new PolyfillGenerator(spec).result

  val defaultTargets = List(
    "INTRINSICS.Array.prototype.at",
    "INTRINSICS.Array.prototype.copyWithin",
    "INTRINSICS.Array.prototype.every",
    "INTRINSICS.Array.prototype.fill",
    "INTRINSICS.Array.prototype.filter",
    "INTRINSICS.Array.prototype.find",
    "INTRINSICS.Array.prototype.findIndex",
    "INTRINSICS.Array.prototype.findLast",
    "INTRINSICS.Array.prototype.findLastIndex",
    "INTRINSICS.Array.prototype.flat",
    "INTRINSICS.Array.prototype.flatMap",
    "INTRINSICS.Array.prototype.forEach",
    "INTRINSICS.Array.prototype.includes",
    "INTRINSICS.Array.prototype.indexOf",
    "INTRINSICS.Array.prototype.join",
    "INTRINSICS.Array.prototype.lastIndexOf",
    "INTRINSICS.Array.prototype.map",
    "INTRINSICS.Array.prototype.pop",
    "INTRINSICS.Array.prototype.push",
    "INTRINSICS.Array.prototype.reduce",
    "INTRINSICS.Array.prototype.reduceRight",
    "INTRINSICS.Array.prototype.reverse",
    "INTRINSICS.Array.prototype.shift",
    "INTRINSICS.Array.prototype.slice",
    "INTRINSICS.Array.prototype.some",
    "INTRINSICS.Array.prototype.splice",
    "INTRINSICS.Array.prototype.toReversed",
    "INTRINSICS.Array.prototype.toSpliced",
    "INTRINSICS.Array.prototype.unshift",
    "INTRINSICS.Array.prototype.with",
  )
}

/** extensible helper of polyfill generator */
class PolyfillGenerator(spec: Spec) {

  import Polyfill.*, PolyfillGenerator.*

  /** generated polyfills */
  lazy val result: List[Polyfill] = for {
    algo <- spec.algorithms
    if (
      algo.isBuiltin &&
      algo.complete &&
      defaultTargets.exists(algo.name.contains)
    )
  } yield compile(algo)

  private val IS_PRESENT = "IsPresent"

  /** compile an algorithm into a polyfill */
  def compile(algo: Algorithm): Polyfill =
    // TODO remove this after implementing all steps
    println(algo)
    println("-" * 80)
    val pb = PolyfillBuilder(spec, algo)

    val name = algo.name.split('.').last
    val belongsTo = algo.name.split('.').init.mkString(".")
    val params = algo.head.originalParams

    val prelude = pb.newScope({
      val shouldInsertIsStrict =
        algo.head.originalParams.forall(_.kind != ParamKind.Variadic)
      if (shouldInsertIsStrict) {
        pb.addStmt(NormalStmt("'use strict';"))
      }

      val shouldInsertIsPresent = hasIsPresentCond(algo.body)
      if (shouldInsertIsPresent) {
        algo.head.originalParams.zipWithIndex.foreach((param, index) => {
          pb.addStmt(
            NormalStmt(
              s"var ${param.name}$IS_PRESENT = arguments.length > $index;",
            ),
          )
        })
      }

      algo.head.originalParams.zipWithIndex
        .foreach((param, index) => {
          if (param.kind == ParamKind.Optional)
            pb.addStmt(
              NormalStmt(
                s"var ${param.name} = arguments.length > $index ? arguments[$index] : undefined;",
              ),
            )
        })
    })

    val body = compileWithScope(pb, algo.body)

    Polyfill(name, params, prelude ++ body, belongsTo)

  def hasIsPresentCond(step: Step): Boolean = {
    var found = false
    val walker = new LangUnitWalker {
      override def walk(cond: Condition): Unit =
        import PredicateConditionOperator.*
        cond match
          case PredicateCondition(_, _, Present) => found = true
          case _                                 =>
    }
    walker.walk(step)
    found
  }

  /** compile with a new scope and convert it into a statement */
  def compileWithScope(pb: PolyfillBuilder, step: Step): Stmt = try {
    pb.newScope(compile(pb, step))
  } catch {
    // TODO remove this catch after implementing all steps
    case e: Throwable =>
      println(pb.currentResult)
      println("-" * 80)
      throw e
  }

  /** compile algorithm steps */
  def compile(
    pb: PolyfillBuilder,
    step: Step,
  ): Unit = step match {
    case LetStep(x, expr) =>
      pb.addStmt(NormalStmt(s"var ${compile(x)} = ${compile(expr)};"))
    case SetStep(x, expr) =>
      pb.addStmt(NormalStmt(s"${compile(x)} = ${compile(expr)};"))
    case SetAsStep(x, verb, id)                   => ???
    case SetEvaluationStateStep(base, func, args) => ???
    case PerformStep(expr) =>
      pb.addStmt(NormalStmt(s"${compile(expr)};"))
    case InvokeShorthandStep(x, a)  => ???
    case AppendStep(expr, ref)      => ???
    case PrependStep(expr, ref)     => ???
    case AddStep(expr, ref)         => ???
    case RemoveStep(t, p, l)        => ???
    case PushContextStep(ref)       => ???
    case SuspendStep(ref, rm)       => ???
    case RemoveContextStep(ctxt, t) => ???
    case AssertStep(cond)           => ()
    case IfStep(cond, thenStep, elseStep, config) =>
      pb.addStmt(
        IfStmt(
          compile(cond),
          compileWithScope(pb, thenStep),
          elseStep.map(compileWithScope(pb, _)),
        ),
      )
    case RepeatStep(cond, body) =>
      import RepeatStep.LoopCondition.*
      val compiledCond = cond match
        case NoCondition => "true"
        case While(cond) => compile(cond)
        case Until(cond) => "!" + compile(cond)
      pb.addStmt(WhileStmt(compiledCond, compileWithScope(pb, body)))
    case ForEachStep(ty, elem, expr, forward, body) =>
      val compiledExpr = compile(expr)
      val index = pb.newTId
      val element = compile(elem)
      val end = s"${compiledExpr}.length"
      val loopHead = NormalStmt(s"var $element = $compiledExpr[$index];")
      val compiledBody = compileWithScope(pb, body)
      pb.addStmt(ForEachStmt(index, end, loopHead ++ compiledBody))
    case ForEachIntegerStep(x, low, lowInc, high, highInc, ascending, body) =>
      ???
    case ForEachOwnPropertyKeyStep(key, obj, cond, ascending, order, body) =>
      ???
    case ForEachParseNodeStep(x, expr, body) => ???
    case ReturnStep(expr) =>
      pb.addStmt(NormalStmt(s"return ${compile(expr)};"))
    case ThrowStep(name) =>
      pb.addStmt(NormalStmt(s"throw new $name;"))
    case ResumeStep(callerCtxt, arg, genCtxt, param, steps) => ???
    case ResumeEvaluationStep(b, aOpt, pOpt, steps)         => ???
    case ResumeTopContextStep()                             => ???
    case NoteStep(note)                                     => ()
    case BlockStep(StepBlock(steps)) =>
      for (substep <- steps) compile(pb, substep.step)
    case YetStep(expr)                          => ???
    case SetFieldsWithIntrinsicsStep(ref, desc) => ???
    case PerformBlockStep(b, d)                 => ???
  }

  /** compile local variable */
  def compile(x: Variable): String = x.name

  /** compile references */
  def compile(ref: Reference): String = ref match {
    case x: Variable               => compile(x)
    case RunningExecutionContext() => ???
    case SecondExecutionContext()  => ???
    case CurrentRealmRecord()      => ???
    case ActiveFunctionObject()    => ???
    case ref: PropertyReference    => compile(ref)
    case AgentRecord()             => ???
  }

  /** compile property references */
  def compile(ref: PropertyReference): String =
    val PropertyReference(base, prop) = ref
    val baseRef = compile(base)
    prop match
      case FieldProperty(name)       => s"$baseRef['$name']"
      case ComponentProperty(name)   => ???
      case BindingProperty(expr)     => ???
      case IndexProperty(index)      => ???
      case IntrinsicProperty(intr)   => ???
      case NonterminalProperty(name) => ???

  /** compile expressions */
  def compile(expr: Expression): String = expr match {
    case StringConcatExpression(exprs)              => compile(exprs, " + ")
    case ListConcatExpression(es)                   => ???
    case ListCopyExpression(expr)                   => ???
    case RecordExpression(rawName, fields)          => ???
    case LengthExpression(ReferenceExpression(ref)) => ???
    case LengthExpression(expr)                     => ???
    case SubstringExpression(expr, from, to)        => ???
    case TrimExpression(expr, leading, trailing)    => ???
    case NumberOfExpression(_, _, expr)  => s"${compile(expr)}.length"
    case IntrinsicExpression(intr)       => ???
    case SourceTextExpression(expr)      => ???
    case CoveredByExpression(code, rule) => ???
    case GetItemsExpression(nt, expr @ NonterminalLiteral(_, name, flags)) =>
      ???
    case expr: GetItemsExpression => ???
    case InvokeAbstractOperationExpression(name, args) =>
      val shouldAppendUnderscore = name match {
        case "Set" => true
        case _     => false
      }
      (if (shouldAppendUnderscore) "_" else "") + s"$name(${compile(args)})"
    case InvokeNumericMethodExpression(ty, name, args)             => ???
    case InvokeAbstractClosureExpression(ref, args)                => ???
    case InvokeMethodExpression(ref, args)                         => ???
    case InvokeSyntaxDirectedOperationExpression(base, name, args) => ???
    case ReturnIfAbruptExpression(expr, _) => compile(expr)
    case ListExpression(entries)           => s"[${compile(entries)}]"
    case IntListExpression(from, fInc, to, tInc, asc) => ???
    case yet: YetExpression                           => ???
    case ReferenceExpression(ref)                     => compile(ref)
    case MathFuncExpression(op, args)   => s"${compile(op)}(${compile(args)})"
    case ConversionExpression(op, expr) => compile(expr)
    case ExponentiationExpression(base, power) =>
      s"pow(${compile(base)}, ${compile(power)})"
    case BinaryExpression(left, op, right) =>
      s"${compile(left)} ${compile(op)} ${compile(right)}"
    case UnaryExpression(op, expr) => s"${compile(op)}${compile(expr)}"
    case ClampExpression(target, lower, upper) =>
      s"clamp(${compile(target)}, ${compile(lower)}, ${compile(upper)})"
    case expr: MathOpExpression                                   => ???
    case BitwiseExpression(left, op, right)                       => ???
    case AbstractClosureExpression(params, captured, body)        => ???
    case XRefExpression(XRefExpressionOperator.Algo, id)          => ???
    case XRefExpression(XRefExpressionOperator.ParamLength, id)   => ???
    case XRefExpression(XRefExpressionOperator.InternalSlots, id) => ???
    case SoleElementExpression(list)                              => ???
    case CodeUnitAtExpression(base, index)                        => ???
    case lit: Literal => compile(lit)
  }

  /** compile iterable of expressions */
  def compile(iterable: Iterable[Expression], sep: String = ", "): String =
    iterable.map(compile(_)).mkString(sep)

  /** compile mathematical operators */
  def compile(expr: MathOpExpression): String = ???

  /** compile binary operators */
  def compile(op: BinaryExpressionOperator): String =
    import BinaryExpressionOperator.*
    op match {
      case Add => "+"
      case Sub => "-"
      case Mul => "*"
      case Div => "/"
      case Mod => "%"
    }

  /** compile unary operators */
  def compile(op: UnaryExpressionOperator): String = op match
    case UnaryExpressionOperator.Neg => "-"

  /** compile bitwise operations */
  def compile(op: BitwiseExpressionOperator): String = ???

  /** compile mathematical function operators */
  def compile(op: MathFuncExpressionOperator): String =
    import MathFuncExpressionOperator.*
    op match {
      case Max      => "max"
      case Min      => "min"
      case Abs      => "abs"
      case Floor    => "floor"
      case Truncate => "truncate"
    }

  /** compile branch conditions */
  def compile(cond: Condition): String = cond match {
    case ExpressionCondition(expr)                 => compile(expr)
    case TypeCheckCondition(expr, neg, tys)        => ???
    case HasFieldCondition(ref, neg, field)        => ???
    case HasBindingCondition(ref, neg, binding)    => ???
    case ProductionCondition(nt, lhsName, rhsName) => ???
    case PredicateCondition(expr, neg, op) =>
      import PredicateConditionOperator.*
      op match {
        case Finite      => ???
        case Abrupt      => ???
        case Throw       => ???
        case Return      => ???
        case Break       => ???
        case Continue    => ???
        case NeverAbrupt => ???
        case Normal      => ???
        case Duplicated  => ???
        case Present     => (if (neg) s"!" else "") + compile(expr) + IS_PRESENT
        case Empty       => ???
        case StrictMode  => ???
        case ArrayIndex  => ???
        case FalseToken  => ???
        case TrueToken   => ???
        case DataProperty     => ???
        case AccessorProperty => ???
        case FullyPopulated   => ???
        case Nonterminal      => ???
      }
    case IsAreCondition(left, neg, right) =>
      val es = for (lexpr <- left) yield {
        val l = compile(lexpr)
        val e = right
          .map(r => s"($l === ${compile(r)})")
          .reduce((l, r) => s"($l || $r)")
        (if (neg) s"!" else "") + e
      }
      es.reduce((l, r) => s"($l && $r)")
    case BinaryCondition(left, op, right) =>
      import BinaryConditionOperator.*
      lazy val l = compile(left)
      lazy val r = compile(right)
      op match {
        case Eq               => s"$l === $r"
        case NEq              => s"$l !== $r"
        case LessThan         => s"$l < $r"
        case LessThanEqual    => s"$l <= $r"
        case GreaterThan      => s"$l > $r"
        case GreaterThanEqual => s"$l >= $r"
        case SameCodeUnits    => ???
      }
    case InclusiveIntervalCondition(left, neg, from, to) => ???
    case ContainsCondition(list, neg, target)            => ???
    case CompoundCondition(left, op, right) =>
      import CompoundConditionOperator.*
      lazy val l = compile(left)
      lazy val r = compile(right)
      op match
        case And   => s"$l && $r"
        case Or    => s"$l || $r"
        case Imply => ???
  }

  def compile(lit: Literal): String =
    lit match {
      case _: ThisLiteral                           => "this"
      case _: NewTargetLiteral                      => ???
      case HexLiteral(hex, name)                    => ???
      case CodeLiteral(code)                        => ???
      case GrammarSymbolLiteral(name, flags)        => ???
      case NonterminalLiteral(ordinal, name, flags) => ???
      case EnumLiteral(name)                        => s"'$name'"
      case StringLiteral(str)                       => s"'$str'"
      case FieldLiteral(name)                       => ???
      case SymbolLiteral(sym)                       => ???
      case ProductionLiteral(lhs, rhs)              => ???
      case ErrorObjectLiteral(name)                 => ???
      case _: PositiveInfinityMathValueLiteral      => "Infinity"
      case _: NegativeInfinityMathValueLiteral      => "-Infinity"
      case DecimalMathValueLiteral(n)               => s"$n"
      case MathConstantLiteral(pre, name)           => ???
      case NumberLiteral(n)                         => s"$n"
      case BigIntLiteral(n)                         => s"${n}n"
      case _: TrueLiteral                           => "true"
      case _: FalseLiteral                          => "false"
      case _: UndefinedLiteral                      => "undefined"
      case _: NullLiteral                           => "null"
      case _: UndefinedTypeLiteral                  => ???
      case _: NullTypeLiteral                       => ???
      case _: BooleanTypeLiteral                    => ???
      case _: StringTypeLiteral                     => ???
      case _: SymbolTypeLiteral                     => ???
      case _: NumberTypeLiteral                     => ???
      case _: BigIntTypeLiteral                     => ???
      case _: ObjectTypeLiteral                     => ???
    }
}
