package esmeta.es.util.polyfill

import esmeta.error.NotSupported
import esmeta.error.NotSupported.Category.Metalanguage
import esmeta.lang.*
import esmeta.spec.*
import esmeta.es.util.polyfill.util.UnitWalker as PolyfillUnitWalker
import esmeta.util.BaseUtils.raise

import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, Stack}

class Translator(spec: Spec) {

  import Polyfill.*

  private val IS_PRESENT = "IsPresent"
  private val AO_HEADER = "AO";
  private val INTERNAL_HEADER = "IN";
  private val RESERVED_WORDS = Set("return")

  /** a metalanguage element the translator does not know how to translate yet
    *
    * The reason names the element and, when the element alone is ambiguous, the
    * parts that make it unsupported (e.g. `LengthExpression/XRefExpression`).
    */
  private def unsupported(elem: Any, details: Any*): Nothing =
    def nameOf(x: Any): String = x match
      case p: Product => p.productPrefix
      case _          => x.getClass.getSimpleName
    throw NotSupported(Metalanguage)(
      "polyfill" :: (elem +: details).map(nameOf).toList,
    )

  def compile(algo: PolyfillAlgo): Polyfill =
    val pb = Builder()

    val name = algo.name
    val params = algo.head.originalParams
    val prelude = compilePrelude(pb, algo.head, algo.body)
    val body = compileWithScope(pb, algo.body)
    Polyfill(name, params, prelude ++ body)

  def compilePrelude(
    pb: Builder,
    head: Head,
    body: PolyfillStep,
  ): Stmt =
    pb.newScope({
      val existenceCheckVariables = {
        var result = mutable.Set[String]()
        new PolyfillUnitWalker {
          override def walk(cond: Condition): Unit =
            import PredicateConditionOperator.*
            cond match
              case PredicateCondition(
                    ReferenceExpression(Variable(name, _, _, _)) :: Nil,
                    _,
                    Present,
                  ) =>
                result += name
              case _ =>
        }.walk(body)
        result.toSet
      }

      head.originalParams.zipWithIndex.foreach((param, index) => {
        if (existenceCheckVariables.contains(param.name))
          pb.addStmt(
            NormalStmt(
              s"var ${param.name}$IS_PRESENT = arguments.length > $index;",
            ),
          )
      })

      head.originalParams.zipWithIndex
        .foreach((param, index) => {
          if (param.kind == ParamKind.Optional)
            pb.addStmt(
              NormalStmt(
                s"var ${param.name} = arguments.length > $index ? arguments[$index] : undefined;",
              ),
            )
        })
    })

  def compileWithScope(pb: Builder, step: Step): Stmt =
    pb.newScope(compile(pb, step))

  def compileWithScope(pb: Builder, step: PolyfillStep): Stmt =
    pb.newScope(compile(pb, step))

  def compile(pb: Builder, step: PolyfillStep): Unit = {
    import PolyfillStep.*
    step match {
      case LangStep(step) => compile(pb, step)
      case Steps(steps)   => steps.foreach(s => compile(pb, s))
      case Let(x, expr) =>
        pb.addStmt(NormalStmt(s"var ${compile(x)} = ${compile(pb, expr)};"))
      case Assign(ref, expr) =>
        pb.addStmt(NormalStmt(s"${compile(pb, ref)} = ${compile(pb, expr)};"))
      case Perform(expr) =>
        pb.addStmt(NormalStmt(s"${compile(pb, expr)};"))
      case Return(expr) =>
        pb.addStmt(NormalStmt(s"return ${compile(pb, expr)};"))
      case Rethrow(name) =>
        pb.addStmt(NormalStmt(s"throw $name;"))
      case Branch(cond, thenStep, elseStep, _) =>
        pb.addStmt(
          IfStmt(
            compile(pb, cond),
            compileWithScope(pb, thenStep),
            elseStep.map(compileWithScope(pb, _)),
          ),
        )
      case check: CompletionCheck =>
        raise(s"completion check survived the rewriting: $check")
      case Wrapped(tryBlock, catchVar, catchBlock) =>
        pb.addStmt(
          TryCatchStmt(
            compileWithScope(pb, tryBlock),
            compile(pb, catchVar),
            compileWithScope(pb, catchBlock.get),
          ),
        )
      case Loop(header, body) => compileLoop(pb, header, body)
    }
  }

  /** compile a loop, taking its shape from the lifted metalanguage header */
  private def compileLoop(
    pb: Builder,
    header: Step,
    body: PolyfillStep,
  ): Unit = header match {
    case RepeatStep(cond, _) =>
      import RepeatStep.LoopCondition.*
      val compiledCond = cond match
        case NoCondition => "true"
        case While(cond) => compile(pb, cond)
        case Until(cond) => "!" + compile(pb, cond)
      pb.addStmt(WhileStmt(compiledCond, compileWithScope(pb, body)))
    case ForEachStep(ty, elem, expr, forward, _) =>
      val compiledExpr = compile(pb, expr)
      val index = pb.newTId
      val element = compile(elem)
      val end = s"${compiledExpr}.length"
      val loopHead = NormalStmt(s"var $element = $compiledExpr[$index];")
      val compiledBody = compileWithScope(pb, body)
      pb.addStmt(ForEachStmt(index, end, loopHead ++ compiledBody))
    case ForEachIntegerStep(x, low, lowInc, high, highInc, ascending, _) =>
      pb.addStmt(
        ForEachIntStmt(
          x.name,
          compile(pb, low),
          lowInc,
          compile(pb, high),
          highInc,
          ascending,
          compileWithScope(pb, body),
        ),
      )
    case other => unsupported(other)
  }

  /** compile the expressions of the polyfill pipeline language */
  def compile(pb: Builder, expr: PolyfillExpr): String = {
    import PolyfillExpr.*
    expr match {
      case LangExpr(expr) => compile(pb, expr)
      case Closure(params, _, body) =>
        val funcBody =
          s"(${params.map(compile).mkString(", ")}) => ${compileWithScope(pb, body)}"
        s"(() => {var _self = $funcBody; return _self;})()"
      case Invoke(name, args, _) =>
        val compiledArgs = args.map(compile(pb, _)).mkString(", ")
        s"${AO_HEADER}__$name($compiledArgs)"
    }
  }

  def compile(
    pb: Builder,
    step: Step,
  ): Unit = step match {
    case LetStep(x, expr) =>
      pb.addStmt(NormalStmt(s"var ${compile(x)} = ${compile(pb, expr)};"))
    case SetStep(x, expr) =>
      pb.addStmt(NormalStmt(s"${compile(pb, x)} = ${compile(pb, expr)};"))
    case SetAsStep(x, verb, id)                   => unsupported(step)
    case SetEvaluationStateStep(base, func, args) => unsupported(step)
    case PerformStep(expr) =>
      pb.addStmt(NormalStmt(s"${compile(pb, expr)};"))
    case InvokeShorthandStep(name, args) => unsupported(step)
    case AppendStep(expr, ref) =>
      pb.addStmt(
        NormalStmt(
          s"${INTERNAL_HEADER}__Append(${compile(pb, ref)}, ${compile(pb, expr)})",
        ),
      )
    case InsertStep(expr, ref) => unsupported(step)
    case PrependStep(expr, ref) =>
      pb.addStmt(
        NormalStmt(
          s"${INTERNAL_HEADER}__Prepend(${compile(pb, ref)}, ${compile(pb, expr)})",
        ),
      )
    case AddStep(expr, ref) => unsupported(step)
    case ReplaceStep(oldElem, newElem, ref) =>
      pb.addStmt(
        NormalStmt(
          s"${INTERNAL_HEADER}__Replace(${compile(pb, ref)}, ${compile(pb, oldElem)}, ${compile(pb, newElem)})",
        ),
      )
    case RemoveStep(t, p, l) =>
      t match {
        case RemoveStep.Target.First(None) =>
          pb.addStmt(NormalStmt(s"${compile(pb, l)}.shift()"))
        case _ => unsupported(step, t)
      }
    case PushContextStep(ref)       => unsupported(step)
    case SuspendStep(ref, rm)       => {}
    case RemoveContextStep(ctxt, t) => unsupported(step)
    case AssertStep(cond)           => ()
    case IfStep(cond, thenStep, elseStep, config) =>
      pb.addStmt(
        IfStmt(
          compile(pb, cond),
          compileWithScope(pb, thenStep),
          elseStep.map(compileWithScope(pb, _)),
        ),
      )
    case RepeatStep(cond, body) =>
      import RepeatStep.LoopCondition.*
      val compiledCond = cond match
        case NoCondition => "true"
        case While(cond) => compile(pb, cond)
        case Until(cond) => "!" + compile(pb, cond)
      pb.addStmt(WhileStmt(compiledCond, compileWithScope(pb, body)))
    case ForEachStep(ty, elem, expr, forward, body) =>
      val compiledExpr = compile(pb, expr)
      val index = pb.newTId
      val element = compile(elem)
      val end = s"${compiledExpr}.length"
      val loopHead = NormalStmt(s"var $element = $compiledExpr[$index];")
      val compiledBody = compileWithScope(pb, body)
      pb.addStmt(ForEachStmt(index, end, loopHead ++ compiledBody))
    case ForEachIntegerStep(x, low, lowInc, high, highInc, ascending, body) =>
      val compiledLow = compile(pb, low)
      val compiledHigh = compile(pb, high)
      val compiledBody = compileWithScope(pb, body)
      pb.addStmt(
        ForEachIntStmt(
          x.name,
          compiledLow,
          lowInc,
          compiledHigh,
          highInc,
          ascending,
          compiledBody,
        ),
      )
    case ForEachOwnPropertyKeyStep(key, obj, cond, ascending, order, body) =>
      unsupported(step)
    case ForEachParseNodeStep(x, expr, body) => unsupported(step)
    case ReturnStep(expr) =>
      pb.addStmt(NormalStmt(s"return ${compile(pb, expr)};"))
    case ThrowStep(name) =>
      pb.addStmt(NormalStmt(s"throw new $name;"))
    case ResumeStep(callerCtxt, arg, genCtxt, param, steps) => unsupported(step)
    case ResumeEvaluationStep(b, aOpt, pOpt, steps)         => unsupported(step)
    case ResumeTopContextStep()                             => unsupported(step)
    case NoteStep(note)                                     => ()
    case BlockStep(StepBlock(steps)) =>
      for (substep <- steps) compile(pb, substep.step)
    case YetStep(expr) => pb.addStmt(NormalStmt(compile(pb, expr)))
    case SetFieldsWithIntrinsicsStep(ref, desc) => unsupported(step)
    case PerformBlockStep(b, d)                 => unsupported(step)
    case MetaStep(name, multiline, _)           => unsupported(step)
  }

  /** compile local variable */
  def compile(x: Variable): String =
    if (RESERVED_WORDS.contains(x.name))
      s"${x.name}_var"
    else
      x.name

  /** compile references */
  def compile(pb: Builder, ref: Reference): String = ref match {
    case x: Variable                => compile(x)
    case Access(base, name, _, _)   => s"${compile(pb, base)}[\"$name\"]"
    case ValueOf(base)              => compile(pb, base)
    case IntrinsicField(base, intr) => unsupported(ref)
    case IndexLookup(base, index) =>
      s"${compile(pb, base)}[${compile(pb, index)}]"
    case BindingLookup(base, binding)   => unsupported(ref)
    case NonterminalLookup(base, nt)    => unsupported(ref)
    case PositionalElement(base, true)  => s"${compile(pb, base)}[0]"
    case PositionalElement(base, false) => unsupported(ref)
    case IntrinsicObject(base, expr)    => unsupported(ref)
    case RunningExecutionContext() => "this" // TODO Single-Runtime Assumption
    case SecondExecutionContext()  => unsupported(ref)
    case CurrentRealmRecord()      => "globalThis"
    case ActiveFunctionObject()    => "_self"
    case AgentRecord()             => unsupported(ref)
    case MetaReference(name, _)    => unsupported(ref)
  }

  /** compile expressions */
  def compile(pb: Builder, expr: Expression): String = expr match {
    case StringConcatExpression(exprs) =>
      exprs
        .map(expr =>
          val e = compile(pb, expr)
          // todo: handle unicode escape sequences properly
          if (e.startsWith("0x")) s"String.fromCharCode($e)" else e,
        )
        .mkString(" + ")
    case ListConcatExpression(es) =>
      s"[].concat(${es.map(compile(pb, _)).mkString(", ")})"
    case CopyExpression(expr, _) => s"${compile(pb, expr)}.slice()"
    case RecordExpression(rawName, fields, form) =>
      s"{${fields.map((fieldLit, fieldExpr) => s"\"${fieldLit.name}\": ${compile(pb, fieldExpr)}").mkString(", ")}}"
    case LengthExpression(ReferenceExpression(ref)) =>
      s"${compile(pb, ref)}.length"
    case LengthExpression(inner) => unsupported(expr, inner)
    case StringExpression(expr)  => compile(pb, expr)
    case SubstringExpression(expr, from, to) =>
      s"${INTERNAL_HEADER}__SubString(${compile(pb, expr)}, ${compile(pb, from)}, ${compile(pb, to)})"
    case TrimExpression(expr, leading, trailing) =>
      s"${INTERNAL_HEADER}__Trim(${compile(pb, expr)}, $leading, $trailing)"
    case NumberOfExpression(_, _, ReferenceExpression(ref), _) =>
      s"${compile(pb, ref)}.length"
    case NumberOfExpression(_, _, inner, _) => unsupported(expr, inner)
    case IntrinsicExpression(intr) =>
      if (intr.props.isEmpty)
        s"${intr.base}"
      else
        s"${intr.base}.${intr.props.mkString(".")}"
    case SourceTextExpression(_)         => unsupported(expr)
    case CoveredByExpression(code, rule) => unsupported(expr)
    case GetItemsExpression(_, inner: NonterminalLiteral) =>
      unsupported(expr, inner)
    case _: GetItemsExpression => unsupported(expr)
    case InvokeAbstractOperationExpression(name, args, tag) =>
      s"${AO_HEADER}__$name(${compile(pb, args)})"
    case InvokeNumericMethodExpression(ty, name, args) =>
      s"${ty}__$name(${compile(pb, args)})"
    case InvokeAbstractClosureExpression(ref, args) =>
      s"${compile(pb, ref)}(${args.map(compile(pb, _)).mkString(", ")})"
    case InvokeMethodExpression(ref, args, tag) =>
      s"${compile(pb, ref)}(${compile(pb, args)})"
    case InvokeSyntaxDirectedOperationExpression(
          base,
          name,
          args,
          prefix,
          tag,
        ) =>
      unsupported(expr)
    case ReturnIfAbruptExpression(expr, _) => compile(pb, expr)
    case ListExpression(form) =>
      import ListExpressionForm.*
      form match
        case LiteralSyntax(entries)         => s"[${compile(pb, entries)}]"
        case SoleElement(entry)             => s"[${compile(pb, entry)}]"
        case EmptyList(isNewUsed, typeDesc) => "[]"
        case IntRange(
              from,
              isFromInclusive,
              to,
              isToInclusive,
              isAscending,
            ) =>
          s"${INTERNAL_HEADER}__IntRange(${compile(pb, from)}, $isFromInclusive, ${compile(pb, to)}, $isToInclusive, $isAscending)"
    case YetExpression(str, block) =>
      s"(function () { throw new Error(\"YET: ${str
        .replace("\\", "\\\\")
        .replace("\"", "\\\"")}\"); })()"
    case ReferenceExpression(ref)     => compile(pb, ref)
    case MathFuncExpression(op, args) => s"${compile(op)}(${compile(pb, args)})"
    case ConversionExpression(ConversionExpressionOperator.ToCodeUnit, e, _) =>
      s"String.fromCharCode(${compile(pb, e)})"
    case ConversionExpression(op, expr, form) => compile(pb, expr)
    case ExponentiationExpression(base, power) =>
      s"Math.pow(${compile(pb, base)}, ${compile(pb, power)})"
    case BinaryExpression(left, op, right, _) =>
      s"${compile(pb, left)} ${compile(op)} ${compile(pb, right)}"
    case UnaryExpression(op, expr) => s"${compile(op)}${compile(pb, expr)}"
    case ClampExpression(target, lower, upper) =>
      s"${INTERNAL_HEADER}__clamp(${compile(pb, target)}, ${compile(pb, lower)}, ${compile(pb, upper)})"
    case MathOpExpression(op, args) =>
      import MathOpExpressionOperator.*
      (op, args) match
        case (Neg, List(e))    => s"-${compile(pb, e)}"
        case (Add, List(l, r)) => s"${compile(pb, l)} + ${compile(pb, r)}"
        case (Mul, List(l, r)) => s"${compile(pb, l)} * ${compile(pb, r)}"
        case (Sub, List(l, r)) => s"${compile(pb, l)} - ${compile(pb, r)}"
        case (Pow, List(l, r)) =>
          s"Math.pow(${compile(pb, l)}, ${compile(pb, r)})"
        case _ => unsupported(expr, op)
    case BitwiseExpression(l, op, r) =>
      s"${compile(pb, l)} ${compile(op)} ${compile(pb, r)}"
    case AbstractClosureExpression(params, captured, body) =>
      val funcBody =
        s"(${params.map(compile).mkString(", ")}) => ${compileWithScope(pb, body)}"
      s"(() => {var _self = $funcBody; return _self;})()"
    case XRefExpression(
          XRefExpressionOperator.Algo | XRefExpressionOperator.Definition |
          XRefExpressionOperator.InternalMethod,
          id,
        ) =>
      val fname = spec
        .getAlgoById(id)
        .head
        .fname
        .stripPrefix("INTRINSICS.yet:")
        .replace("`", "")
        .replace(".", "")
      s"${AO_HEADER}__${fname}"
    case XRefExpression(XRefExpressionOperator.ParamLength, id) =>
      spec.getAlgoById(id).head.originalParams.length.toString
    case XRefExpression(kind, id)    => unsupported(expr)
    case SoleElementExpression(list) => unsupported(expr)
    case CodeUnitAtExpression(base, index) =>
      s"${compile(pb, base)}.charCodeAt(${compile(pb, index)})"
    case lit: Literal            => compile(lit)
    case MetaExpression(name, _) => unsupported(expr)
  }

  /** compile iterable of expressions */
  def compile(
    pb: Builder,
    iterable: Iterable[Expression],
    sep: String = ", ",
  ): String =
    iterable.map(compile(pb, _)).mkString(sep)

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
  def compile(op: BitwiseExpressionOperator): String = op match
    case BitwiseExpressionOperator.BAnd => "&"
    case BitwiseExpressionOperator.BOr  => "|"
    case BitwiseExpressionOperator.BXOr => "^"

  /** compile mathematical function operators */
  def compile(op: MathFuncExpressionOperator): String =
    import MathFuncExpressionOperator.*
    op match {
      case Max      => s"Math.max"
      case Min      => s"Math.min"
      case Abs      => unsupported(op)
      case Floor    => s"Math.floor"
      case Truncate => s"Math.trunc"
      case Log10    => unsupported(op)
      case Log2     => unsupported(op)
      case Log      => unsupported(op)
    }

  def compileTypeCheck(expr: String, ty: String): String = ty match
    case "record[object]" | "object" => s"${AO_HEADER}__IsObject($expr)"
    case "record[symbol]"            => s"typeof $expr === \"symbol\""
    case "numberint"     => s"${INTERNAL_HEADER}__IsIntegralNumber($expr)"
    case "record[array]" => s"${AO_HEADER}__IsArray($expr)"
    case _ if ty.startsWith("record[") => "false"
    case _                             => s"typeof $expr === \"$ty\""

  def negateIf(neg: Boolean)(cond: String): String =
    if (neg) s"!($cond)" else s"($cond)"

  /** compile branch conditions */
  def compile(pb: Builder, cond: Condition): String = cond match {
    case ExpressionCondition(expr) => compile(pb, expr)
    case TypeCheckCondition(expr, neg, tys) =>
      val compiledExpr = compile(pb, expr)
      val tyNames = tys.map(_.normalizedName.toLowerCase())
      if (tyNames.length == 1)
        negateIf(neg)(compileTypeCheck(compiledExpr, tyNames.head))
      else
        val operand = pb.newTId
        val checks =
          tyNames.map(compileTypeCheck(operand, _)).mkString("(", "||", ")")
        negateIf(neg)(
          s"(function ($operand) { return $checks; })($compiledExpr)",
        )
    case HasFieldCondition(ref, neg, field, form, opTy) =>
      negateIf(neg)(s"${compile(pb, field)} in ${compile(pb, ref)}")
    case HasBindingCondition(ref, neg, binding)    => unsupported(cond)
    case ProductionCondition(nt, lhsName, rhsName) => unsupported(cond)
    case PredicateCondition(expr, neg, op) =>
      import PredicateConditionOperator.*
      op match {
        case Finite  => negateIf(neg)(s"isFinite(${compile(pb, expr)})")
        case Present => negateIf(neg)(compile(pb, expr) + IS_PRESENT)
        case x       => s"TODO: $x"
      }
    case IsAreCondition(left, neg, right) =>
      val es = for (lexpr <- left) yield {
        val l = compile(pb, lexpr)
        val e = right
          .map {
            // The specification asks whether the value *is* NaN, whereas the
            // global isNaN coerces first and would report every non-numeric
            // string as NaN. Self-inequality holds for NaN and nothing else.
            case NumberLiteral(n) if n.isNaN => s"$l !== $l"
            case rexpr                       => s"$l === ${compile(pb, rexpr)}"
          }
          .reduce((l, r) => s"$l || $r")
        negateIf(neg)(e)
      }
      es.reduce((l, r) => s"($l && $r)")
    case BinaryCondition(left, op, right) =>
      import BinaryConditionOperator.*
      lazy val l = compile(pb, left)
      lazy val r = compile(pb, right)
      op match {
        case Eq               => s"$l === $r"
        case NEq              => s"$l !== $r"
        case LessThan         => s"$l < $r"
        case LessThanEqual    => s"$l <= $r"
        case GreaterThan      => s"$l > $r"
        case GreaterThanEqual => s"$l >= $r"
        case SameCodeUnits    => unsupported(cond, op)
      }
    case InclusiveIntervalCondition(left, neg, from, to, _) =>
      val l = compile(pb, left)
      negateIf(neg)(s"$l >= ${compile(pb, from)} && $l <= ${compile(pb, to)}")
    case ContainsCondition(list, neg, ContainsConditionTarget.Expr(target)) =>
      negateIf(neg)(
        s"${INTERNAL_HEADER}__Contains(${compile(pb, list)}, ${compile(pb, target)})",
      )
    case ContainsCondition(_, _, target) => unsupported(cond, target)
    case CompoundCondition(left, op, right) =>
      import CompoundConditionOperator.*
      lazy val l = compile(pb, left)
      lazy val r = compile(pb, right)
      op match
        case And   => s"$l && $r"
        case Or    => s"$l || $r"
        case Imply => unsupported(cond, op)
    case MetaCondition(name, _) => unsupported(cond)
  }

  def compile(lit: Literal): String =
    lit match {
      case _: ThisLiteral          => "this"
      case _: ThisParseNodeLiteral => unsupported(lit)
      case _: NewTargetLiteral     => "new.target"
      // A hex literal is a bare code point where the specification compares
      // numbers, and the character it names where the specification calls it a
      // code unit -- "the code unit 0x0020 (SPACE)" means the string " ".
      case HexLiteral(hex, hasCodeUnitDescription, _, _) =>
        val value = s"0x${hex.toHexString.toUpperCase}"
        if (hasCodeUnitDescription) s"String.fromCharCode($value)" else value
      case CodeLiteral(code)                 => s"\"$code\""
      case ConstantLiteral(name)             => unsupported(lit)
      case GrammarSymbolLiteral(name, flags) => unsupported(lit)
      case NonterminalLiteral(ordinal, name, flags, hasArticle) =>
        unsupported(lit)
      case EnumLiteral(name)           => s"\"$name\""
      case StringLiteral(str, _)       => s"\"$str\""
      case FieldLiteral(name)          => s"\"$name\""
      case SymbolLiteral(sym)          => s"Symbol.$sym"
      case ProductionLiteral(lhs, rhs) => unsupported(lit)
      case ErrorObjectLiteral(name) =>
        name match {
          case "AggregateError" => s"new $name(errors)"
          case _                => s"new $name()"
        }
      case _: PositiveInfinityMathValueLiteral => "Infinity"
      case _: NegativeInfinityMathValueLiteral => "-Infinity"
      case DecimalMathValueLiteral(n)          => s"$n"
      case MathConstantLiteral(pre, name)      => unsupported(lit)
      case NumberLiteral(n)        => if (n.toInt == n) s"${n.toInt}" else s"$n"
      case BigIntLiteral(n)        => s"${n}n"
      case _: TrueLiteral          => "true"
      case _: FalseLiteral         => "false"
      case _: UndefinedLiteral     => "undefined"
      case _: NullLiteral          => "null"
      case _: UndefinedTypeLiteral => unsupported(lit)
      case _: NullTypeLiteral      => unsupported(lit)
      case _: BooleanTypeLiteral   => unsupported(lit)
      case _: StringTypeLiteral    => unsupported(lit)
      case _: SymbolTypeLiteral    => unsupported(lit)
      case _: NumberTypeLiteral    => unsupported(lit)
      case _: BigIntTypeLiteral    => unsupported(lit)
      case _: ObjectTypeLiteral    => unsupported(lit)
    }
}

/** accumulator for the JavaScript statements of one polyfill
  *
  * Statements are emitted into the innermost open scope; [[newScope]] opens one
  * and returns everything emitted inside it as a single block.
  */
case class Builder() {
  import Polyfill.*

  /** create a new scope with a given procedure */
  def newScope(doit: => Unit): Stmt =
    scopes.push(ListBuffer())
    doit
    BlockStmt(scopes.pop.toList)

  /** add JS statements to the current scope */
  def addStmt(stmts: Stmt*): Unit = scopes.head ++= stmts
    .flatMap {
      case BlockStmt(is) => is
      case i             => List(i)
    }

  /** get next temporal variable */
  def newTId: String = s"_x$nextTId"

  private def nextTId: Int = { val tid = tidCount; tidCount += 1; tid }
  private var tidCount: Int = 0
  private var scopes: Stack[ListBuffer[Stmt]] = Stack()
}
