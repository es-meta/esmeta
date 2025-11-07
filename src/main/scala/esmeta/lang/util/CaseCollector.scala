package esmeta.lang.util

import esmeta.lang.*
import esmeta.util.BaseUtils.{*, given}
import scala.collection.mutable.{Map => MMap, ListBuffer}

/** a case collector for metalanguage */
class CaseCollector extends UnitWalker {
  import CaseCollector.*

  val steps: MMap[String, MMap[String, ListBuffer[Step]]] = MMap()
  val exprs: MMap[String, MMap[String, ListBuffer[Expression]]] = MMap()
  val conds: MMap[String, MMap[String, ListBuffer[Condition]]] = MMap()
  val refs: MMap[String, MMap[String, ListBuffer[Reference]]] = MMap()

  def getAdd[T](
    map: MMap[String, MMap[String, ListBuffer[T]]],
    t: T,
  ): String => Unit = { str =>
    map
      .getOrElseUpdate(t.getClass.getSimpleName, MMap())
      .getOrElseUpdate(str, ListBuffer()) += t
  }

  override def walk(step: Step): Unit = {
    val add = getAdd(steps, step)
    add(step match {
      case LetStep(x, expr) =>
        s"let {{ var }} be {{ expr }}."
      case SetStep(x, expr) =>
        s"set {{ ref }} to {{ expr }}."
      case SetAsStep(x, verb, id) =>
        s"set {{ ref }} as $verb in <emu-xref href=\"#{{ str }}\"></emu-xref>."
      case SetEvaluationStateStep(context, func, args) =>
        s"set the code evaluation state of {{ ref }} such that when evaluation is resumed for that execution context, {{ var }} will be called {{ args }}."
      case PerformStep(expr) =>
        s"perform {{ expr }}."
      case InvokeShorthandStep(name, args) =>
        s"{{ str }}({{ expr }}*)."
      case AppendStep(expr, ref) =>
        s"append {{ expr }} to {{ ref }}."
      case PrependStep(expr, ref) =>
        s"prepend {{ expr }} to {{ ref }}."
      case InsertStep(expr, ref) =>
        s"insert {{ expr }} as the first element of {{ ref }}."
      case AddStep(expr, ref) =>
        s"add {{ expr }} to {{ ref }}."
      case RemoveStep(target, prep, list) =>
        import RemoveStep.Target.*
        target match
          case First(Some(e)) =>
            s"remove the first {{ expr }} elements $prep {{ expr }}."
          case First(None) =>
            s"remove the first element $prep {{ expr }}."
          case Last(Some(e)) =>
            s"remove the last {{ expr }} elements $prep {{ expr }}."
          case Last(None) =>
            s"remove the last element $prep {{ expr }}."
          case Element(e) =>
            s"remove {{ expr }} $prep {{ expr }}."
      case PushContextStep(ref) =>
        s"push {{ ref }} onto the execution context stack; {{ ref }} is now the running execution context."
      case SuspendStep(xOpt, remove) =>
        xOpt match
          case Some(x) if remove =>
            s"suspend {{ var }} and remove it from the execution context stack."
          case Some(x) =>
            s"suspend {{ var }}."
          case None if remove =>
            s"suspend the running execution context and remove it from the execution context stack."
          case None =>
            s"suspend the running execution context."
      case RemoveContextStep(context, restoreTarget) =>
        import RemoveContextStep.RestoreTarget.*
        restoreTarget match
          case NoRestore =>
            s"remove {{ ref }} from the execution context stack."
          case StackTop =>
            s"remove {{ ref }} from the execution context stack and restore the execution context that is at the top of the execution context stack as the running execution context."
          case Context(ref) =>
            s"remove {{ ref }} from the execution context stack and restore {{ ref }} as the running execution context."
      case AssertStep(cond) =>
        s"assert: {{ cond }}."
      case IfStep(cond, thenStep, elseStep, config) =>
        val IfStep.ElseConfig(newLine, keyword, comma) = config

        val e = thenStep.endingChar
        val k = if (thenStep.isNextLowercase) keyword else keyword.toFirstUpper
        val n = if (newLine) "<NEWLINE> " else ""
        val c = if (comma) "," else ""

        s"if {{ cond }}, {{ step }}$e $n$k$c {{ step }}."
      case RepeatStep(cond, body) =>
        import RepeatStep.LoopCondition.*
        cond match
          case NoCondition => s"repeat, {{ step }}"
          case While(c)    => s"repeat, while {{ cond }}, {{ step }}"
          case Until(c)    => s"repeat, until {{ cond }}, {{ step }}"
      case ForEachStep(ty, elem, expr, forward, body) =>
        ty match
          case Some(ty) if forward =>
            s"for each {{ ty }} {{ var }} of {{ expr }}, {{ step }}"
          case Some(ty) =>
            s"for each {{ ty }} {{ var }} of {{ expr }}, in reverse List order, {{ step }}"
          case None if forward =>
            s"for each element {{ var }} of {{ expr }}, {{ step }}"
          case None =>
            s"for each element {{ var }} of {{ expr }}, in reverse List order, {{ step }}"
      case ForEachIntegerStep(x, low, lowInc, high, highInc, ascending, body) =>
        def op(inc: Boolean): String = if (inc) "≤" else "<"
        val asc = if (ascending) "ascending" else "descending"
        s"for each integer {{ var }} such that {{ expr }} ${op(lowInc)} {{ var }} ${op(highInc)} {{ expr }}, in $asc order, {{ step }}"
      case ForEachOwnPropertyKeyStep(key, obj, cond, ascending, order, body) =>
        import ForEachOwnPropertyKeyStepOrder.*
        val asc = if (ascending) "ascending" else "descending"
        val ord = order match
          case NumericIndexOrder  => "numeric index order"
          case ChronologicalOrder => "chronological order of property creation"
        s"for each own property key {{ var }} of {{ var }} such that {{ cond }}, in $asc $ord, {{ step }}"
      case ForEachParseNodeStep(x, expr, body) =>
        s"for each child node {{ var }} of {{ expr }}, {{ step }}"
      case ReturnStep(expr) =>
        s"return {{ expr }}."
      case ThrowStep(name) =>
        s"throw ${("*" + name + "*").withIndefArticle} exception."
      case ResumeStep(callerCtxt, arg, genCtxt, param, steps) =>
        s"resume {{ ref }} passing {{ expr }}. if {{ ref }} is ever resumed again, let {{ var }} be the Completion Record with which it is resumed. {{ step }}"
      case ResumeEvaluationStep(context, argOpt, paramOpt, body) =>
        val a = argOpt.fold("") { _ =>
          s"using {{ expr }} as the result of the operation that suspended it"
        }
        val p = paramOpt.fold("") { (_, kind) =>
          s" let {{ var }} be the $kind returned by the resumed computation."
        }
        s"resume the suspended evaluation of {{ ref }} $a.$p {{ step }}"
      case ResumeTopContextStep() =>
        s"resume the context that is now on the top of the execution context stack as the running execution context."
      case NoteStep(note) =>
        s"NOTE: {{ str }}"
      case BlockStep(block) =>
        s"[...]."
      case YetStep(expr) =>
        s"[...]."
      case SetFieldsWithIntrinsicsStep(ref, desc) =>
        s"set fields of {{ ref }} with the values listed in ..."
      case PerformBlockStep(block, desc) =>
        s"perform the following substeps in an implementation-defined order ..."
    })
    super.walk(step)
  }

  override def walk(expr: Expression): Unit = {
    val add = getAdd(exprs, expr)
    import ConversionExpressionForm.*
    add(expr match {
      case StringConcatExpression(exprs) =>
        "the string-concatenation of {{ expr }}*"
      case ListConcatExpression(exprs) =>
        "the list-concatenation of {{ expr }}*"
      case ListCopyExpression(expr) =>
        "a List whose elements are the elements of {{ expr }}"
      case RecordExpression(ty, fields, form) =>
        import RecordExpressionForm.*
        form match {
          case SyntaxLiteral(prefix) =>
            val pre = prefix.fold("")(_ + " ")
            s"$pre{{ ty }} { [ {{ field }}: {{ expr }} ]* }"
          case Text =>
            "a new {{ ty }} whose {{ field }} is {{ expr }}"
          case TextWithNoElement(prefix, postfix) =>
            val pre = prefix + " "
            val post = postfix.fold("")(" " + _)
            s"$pre{{ ty }}$post"
        }
      case LengthExpression(expr) =>
        "the length of {{ expr }}"
      case SubstringExpression(expr, from, to) =>
        val t = to.fold("")(_ => " to {{ expr }}")
        s"the substring of {{ expr }} from {{ expr }}$t"
      case TrimExpression(expr, leading, trailing) =>
        val str = (leading, trailing) match
          case (true, true)   => "both leading and trailing"
          case (true, false)  => "leading"
          case (false, true)  => "trailing"
          case (false, false) => "no"
        s"the String value that is a copy of {{ expr }} with $str whitespace removed"
      case NumberOfExpression(name, pre, expr, exclude) =>
        val p = pre.fold("")("the " + _ + " ")
        val e =
          if (exclude.isDefined) ", excluding all occurrences of {{ expr }}"
          else ""
        s"the number of $name in $p{{ expr }}$e"
      case SourceTextExpression(expr) =>
        "the source text matched by {{ expr }}"
      case CoveredByExpression(code, rule) =>
        "the {{ expr }} that is covered by {{ expr }}"
      case GetItemsExpression(nt, expr) =>
        s"the List of {{ expr }} items in {{ expr }}, in source text order"
      case IntrinsicExpression(intr) =>
        s"%{{ str }}%"
      case XRefExpression(op, id) =>
        import XRefExpressionOperator.*
        val o = op match
          case Algo       => "the definition specified in"
          case Definition => "the algorithm steps defined in"
          case InternalMethod =>
            "the ordinary object internal method defined in"
          case InternalSlots => "the internal slots listed in"
          case ParamLength =>
            "the number of non-optional parameters of the function definition in"
        s"$o <emu-xref href=\"#{{ str }}\"></emu-xref>."
      case ReturnIfAbruptExpression(expr, check) =>
        val op = if (check) "?" else "!"
        s"$op {{ expr }}"
      case ReferenceExpression(ref) =>
        "{{ ref }}"
      case MathFuncExpression(op, args) =>
        s"$op({{ expr }}*)"
      case ConversionExpression(op, expr, SyntaxLiteral) =>
        s"$op({{ expr }})"
      case ConversionExpression(op, expr, Text(a, pre)) =>
        import ConversionExpressionOperator.*
        val opStr = op match
          case ToNumber       => "Number"
          case ToBigInt       => "BigInt"
          case ToMath         => "Math"
          case ToApproxNumber => "implementation-approximated Number"
          case ToCodeUnit     => "code unit whose numeric value is"
        s"$a $opStr value $pre {{expr}}"
      case ExponentiationExpression(base, power) =>
        s"{{ expr }} <sup>{{ expr }}</sup>"
      case BinaryExpression(left, op, right) =>
        s"{{ expr }} $op {{ expr }}"
      case UnaryExpression(op, expr) =>
        s"$op {{ expr }}"
      case ThisLiteral(article) =>
        val a = if (article) "the " else ""
        s"$a*this* value"
      case ThisParseNodeLiteral(nt) =>
        nt match
          case None     => s"this Parse Node"
          case Some(nt) => s"this |{{ str }}|"
      case _: NewTargetLiteral =>
        s"NewTarget"
      case HexLiteral(hex, unicode, codeunit, name) =>
        val codeUnitStr = if (codeunit) "the code unit " else ""
        val pre = if (unicode) "U+" else "0x"
        val nameStr = if (name.isDefined) " (NAME)" else ""
        f"$codeUnitStr${pre}XXXX$nameStr"
      case CodeLiteral(code) =>
        s"`{{ str }}`"
      case GrammarSymbolLiteral(name, flags) =>
        s"the grammar symbol |{{ str }}|"
      case NonterminalLiteral(ordinal, name, flags, article) =>
        val pre = ordinal match
          case Some(value) => ordinal.fold("")(s"the " + _.toOrdinal + " ")
          case None        => if (article) "the " else ""
        s"$pre|{{ str }}|"
      case EnumLiteral(name) =>
        s"~{{ str }}~"
      case StringLiteral(str, form) =>
        import StringLiteralForm.*
        form match {
          case SyntaxLiteral => "*\"{{ str }}\"*"
          case EmptyString   => "the empty String"
          case EmptyUnicode  => "the empty sequence of Unicode code points"
          case Code          => "<code>{{ str }}</code>"
        }
      case FieldLiteral(name) =>
        s"[[{{ str }}]]"
      case SymbolLiteral(sym) =>
        s"%Symbol.{{ str }}%"
      case ProductionLiteral(lhs, rhs) =>
        s"<emu-grammar>{{ str }} : {{ str }}</emu-grammar>"
      case ErrorObjectLiteral(name) =>
        s"a newly created *{{ str }}* object"
      case _: PositiveInfinityMathValueLiteral =>
        s"+∞"
      case _: NegativeInfinityMathValueLiteral =>
        s"-∞"
      case DecimalMathValueLiteral(n) =>
        s"{{ decimal }}"
      case MathConstantLiteral(pre, name) =>
        s"{{ const }}"
      case NumberLiteral(n) =>
        s"{{ number }}"
      case BigIntLiteral(n) =>
        s"*{{ int }}*<sub>ℤ</sub>"
      case _: TrueLiteral =>
        s"*true*"
      case _: FalseLiteral =>
        s"*false*"
      case _: UndefinedLiteral =>
        s"*undefined*"
      case _: NullLiteral =>
        s"*null*"
      case _: UndefinedTypeLiteral =>
        s"Undefined"
      case _: NullTypeLiteral =>
        s"Null"
      case _: BooleanTypeLiteral =>
        s"Boolean"
      case _: StringTypeLiteral =>
        s"String"
      case _: SymbolTypeLiteral =>
        s"Symbol"
      case _: NumberTypeLiteral =>
        s"Number"
      case _: BigIntTypeLiteral =>
        s"BigInt"
      case _: ObjectTypeLiteral =>
        s"Object"
      case ClampExpression(target, lower, upper) =>
        s"the result of clamping {{ expr }} between {{ expr }} and {{ expr }}"
      case MathOpExpression(op, args) =>
        s"$op ..."
      case BitwiseExpression(left, op, right) =>
        s"the result of applying the $op to {{ expr }} and {{ expr }}"
      case InvokeAbstractOperationExpression(name, args, tag) =>
        tag match
          case HtmlTag.None => s"{{ str }}({{ expr }}*)"
          case HtmlTag.BeforeCall(c) =>
            s"<emu-meta>{{ str }}</emu-meta>({{ expr }}*)"
          case HtmlTag.AfterCall(c) =>
            s"<emu-meta>{{ str }}({{ expr }}*)</emu-meta>"
      case InvokeNumericMethodExpression(base, name, args) =>
        s"{{ str }}::{{ str }}({{ expr }}*)"
      case InvokeAbstractClosureExpression(x, args) =>
        s"{{ var }}({{ expr }}*)"
      case InvokeMethodExpression(base, args, tag) =>
        tag match
          case HtmlTag.None => s"{{ str }}({{ expr }}*)"
          case HtmlTag.BeforeCall(c) =>
            s"<emu-meta>{{ str }}</emu-meta>({{ expr }}*)"
          case HtmlTag.AfterCall(c) =>
            s"<emu-meta>{{ str }}({{ expr }}*)</emu-meta>"
      case InvokeSyntaxDirectedOperationExpression(
            base,
            name,
            args,
            article,
            tag,
          ) =>
        val a = article.fold("")(_ + " ")
        if (name == "Contains")
          s"{{ expr }} Contains {{ expr }}"
        else if (args.isEmpty)
          s"$a{{ str }} of {{ expr }} with no arguments"
        else
          s"$a{{ str }} of {{ expr }} with argument(s) {{ expr }}*"
      case ListExpression(form) =>
        import ListExpressionForm.*
        form match
          case LiteralSyntax(e) =>
            e match
              case Nil => "« »"
              case _   => s"« {{ expr }}* »"
          case SoleElement(e) =>
            "a List whose sole element is {{ expr }}"
          case EmptyList(isNewUsed, typeDesc) =>
            val a = if (isNewUsed) "a new" else "an"
            val t = if (typeDesc.isDefined) " of " + typeDesc.get else ""
            s"$a empty List$t"
          case IntRange(from, isFromInc, to, isToInc, isInc) =>
            val from = if (isFromInc) "inclusive" else "exclusive"
            val to = if (isToInc) "inclusive" else "exclusive"
            val asc = if (isInc) "ascending" else "descending"
            s"a List of the integers in the interval from {{ expr }} ($from) to {{ expr }} ($to), in $asc order"
      case SoleElementExpression(expr) =>
        s"the sole element of {{ expr }}"
      case CodeUnitAtExpression(base, index) =>
        s"the code unit at index {{ expr }} within {{ expr }}"
      case StringExpression(expr) =>
        s"the String value {{ expr }}"
      case YetExpression(str, block) =>
        s"..."
      case AbstractClosureExpression(params, captured, body) =>
        val p =
          if (params.isEmpty) "no parameters"
          else "parameters ({{ var }}*)"
        s"a new Abstract Closure with $p that captures {{ var }}* and performs the following steps when called: {{ step }}"
    })
    super.walk(expr)
  }

  override def walk(cond: Condition): Unit = {
    val add = getAdd(conds, cond)
    add(cond match {
      case ExpressionCondition(expr) =>
        s"{{ expr }}"
      case TypeCheckCondition(expr, neg, ty) =>
        if (neg) s"{{ expr }} is not {{ ty }}*"
        else s"{{ expr }} is {{ ty }}*"
      case HasFieldCondition(ref, neg, field, form, tyOpt) =>
        val h = if (neg) "does not have" else "has"
        val f = form match
          case HasFieldConditionForm.Field          => "field"
          case HasFieldConditionForm.InternalMethod => "internal method"
          case HasFieldConditionForm.InternalSlot   => "internal slot"
        val post = tyOpt.fold("")(_ => " whose value is {{ ty }}")
        if (field.length > 1) s"{{ ref }} $h {{ field }}* $f$post"
        else s"{{ ref }} $h a {{ field }} $f$post"
      case HasBindingCondition(ref, neg, binding) =>
        val h = if (neg) "does not have" else "has"
        s"{{ ref }} $h a binding for {{ binding }}"
      case ProductionCondition(nt, lhs, rhs) =>
        s"{{ expr }} is <emu-grammar>{{ str }} : {{ str }}</emu-grammar>"
      case PredicateCondition(x, neg, op) =>
        if (neg) s"{{ expr }} is not $op"
        else s"{{ expr }} is $op"
      case IsAreCondition(ls, neg, rs) =>
        if (neg) s"{{ expr }}* is/are not {{ expr }}*"
        else s"{{ expr }}* is/are {{ expr }}*"
      case BinaryCondition(left, op, right) =>
        s"{{ expr }} $op {{ expr }}"
      case InclusiveIntervalCondition(left, neg, from, to, isTextForm) =>
        if (isTextForm)
          val n = if (neg) " not" else ""
          s"{{ expr }} is${n} in the inclusive interval from {{ expr }} to {{ expr }}"
        else s"{{ expr }} ≤ {{ expr }} ≤ {{ expr }}"
      case ContainsCondition(list, neg, expr) =>
        val c = if (neg) "does not contain" else "contains"
        s"{{ expr }} $c {{ expr }}"
      case CompoundCondition(left, CompoundConditionOperator.Imply, right) =>
        s"if {{ expr }}, then {{ expr }}"
      case CompoundCondition(left, op, right) =>
        s"{{ expr }} $op {{ expr }}"
    })
    super.walk(cond)
  }

  override def walk(ref: Reference): Unit = {
    val add = getAdd(refs, ref)
    import AccessKind.*, AccessForm.*
    def str(kind: AccessKind): String = kind match
      case Field            => "[[ {{ str }} ]]"
      case Component(false) => "{{ str }}"
      case Component(true)  => "{{ str }} component"
    add(ref match {
      case Variable(_, None) =>
        "_{{ str }}_"
      case Variable(_, Some(_)) =>
        "|{{ str }}| _{{ str }}_"
      case Access(_, _, kind, Dot) =>
        s"{{ ref }}.${str(kind)}"
      case Access(_, _, kind, Of) =>
        s"the ${str(kind)} of {{ ref }}"
      case Access(_, _, kind, Apo(None)) =>
        s"{{ ref }}'s ${str(kind)}"
      case Access(_, _, kind, Apo(Some(desc))) =>
        s"{{ ref }}'s ${str(kind)} $desc"
      case ValueOf(_) =>
        "the value of {{ ref }}"
      case IntrinsicField(_, _) =>
        "{{ ref }}.[[ %{{ str }}% ]]"
      case IndexLookup(_, _) =>
        "{{ ref }}[ {{ expr }} ]"
      case BindingLookup(_, _) =>
        "the binding for {{ expr }} in {{ expr }}"
      case NonterminalLookup(_, _) =>
        "the |{{ str }}| of {{ ref }}"
      case PositionalElement(_, true) =>
        "the first element of {{ ref }}"
      case PositionalElement(_, false) =>
        "the last element of {{ ref }}"
      case IntrinsicObject(base, expr) =>
        "{{ ref }}'s intrinsic object named {{ expr }}"
      case _: RunningExecutionContext =>
        "the running execution context"
      case _: SecondExecutionContext =>
        "the second to top element of the execution context stack"
      case _: CurrentRealmRecord =>
        "the current Realm Record"
      case _: ActiveFunctionObject =>
        "the active function object"
      case AgentRecord() =>
        "the Agent Record of the surrounding agent"
    })
    super.walk(ref)
  }
}

object CaseCollector {
  enum Kind { case Step, Expr, Cond, Etc }
}
