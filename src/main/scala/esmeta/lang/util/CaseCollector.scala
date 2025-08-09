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
        (newLine, comma) match
          case (true, true) =>
            s"if {{ cond }}, {{ step }}. <NEWLINE> $keyword, {{ step }}."
          case (true, false) =>
            s"if {{ cond }}, {{ step }}. <NEWLINE> $keyword {{ step }}."
          case (false, true) =>
            s"if {{ cond }}, {{ step }}. $keyword, {{ step }}."
          case (false, false) =>
            s"if {{ cond }}, {{ step }}. $keyword {{ step }}."
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
      case BlockEarlyErrorDefsStep(cond) => "TODO"
    })
    super.walk(step)
  }

  override def walk(expr: Expression): Unit = {
    val add = getAdd(exprs, expr)
    import ConversionExpressionOperator.*
    add(expr match {
      case StringConcatExpression(exprs) =>
        s"the string-concatenation of {{ expr }}*"
      case ListConcatExpression(exprs) =>
        s"the list-concatenation of {{ expr }}*"
      case ListCopyExpression(expr) =>
        s"a List whose elements are the elements of {{ expr }}"
      case RecordExpression(ty, fields) =>
        s"{{ ty }} { [ {{ field }}: {{ expr }} ]* }"
      case LengthExpression(expr) =>
        s"the length of {{ expr }}"
      case SubstringExpression(expr, from, to) =>
        to match
          case Some(to) =>
            s"the substring of {{ expr }} from {{ expr }} to {{ expr }}"
          case None =>
            s"the substring of {{ expr }} from {{ expr }}"
      case TrimExpression(expr, leading, trailing) =>
        val str = (leading, trailing) match
          case (true, true)   => "both leading and trailing"
          case (true, false)  => "leading"
          case (false, true)  => "trailing"
          case (false, false) => "no"
        s"the String value that is a copy of {{ expr }} with $str whitespace removed"
      case NumberOfExpression(expr) =>
        s"the number of elements in {{ expr }}"
      case SourceTextExpression(expr) =>
        s"the source text matched by {{ expr }}"
      case CoveredByExpression(code, rule) =>
        s"the {{ expr }} that is covered by {{ expr }}"
      case GetItemsExpression(nt, expr) =>
        s"the List of items in {{ expr }}, in source text order"
      case IntrinsicExpression(intr) =>
        s"%{{ str }}%"
      case XRefExpression(kind, id) =>
        s"<emu-xref href=\"#{{ str }}\"></emu-xref>."
      case ReturnIfAbruptExpression(expr, check) =>
        val op = if (check) "?" else "!"
        s"$op {{ expr }}"
      case ReferenceExpression(ref) =>
        s"{{ ref }}"
      case MathFuncExpression(op, args) =>
        s"$op({{ expr }}*)"
      case ConversionExpression(ToApproxNumber, expr) =>
        s"an implementation-approximated Number value representing {{ expr }}"
      case ConversionExpression(o, e: (CalcExpression | InvokeExpression)) =>
        s"$o({{ expr }})"
      case ConversionExpression(op, expr) =>
        s"the $op value of {{ expr }}"
      case ExponentiationExpression(base, power) =>
        s"{{ expr }} <sup>{{ expr }}</sup>"
      case BinaryExpression(left, op, right) =>
        s"{{ expr }} $op {{ expr }}"
      case UnaryExpression(op, expr) =>
        s"$op {{ expr }}"
      case _: ThisLiteral =>
        s"*this* value"
      case _: NewTargetLiteral =>
        s"NewTarget"
      case HexLiteral(hex, name) =>
        val desc = name.fold("")(" (" + _ + ")")
        f"0x$hex%04x$desc"
      case CodeLiteral(code) =>
        s"`{{ str }}`"
      case GrammarSymbolLiteral(name, flags) =>
        s"the grammar symbol |{{ str }}|"
      case NonterminalLiteral(ordinal, name, flags) =>
        val ord = ordinal.fold("")(s"the " + _.toOrdinal + " ")
        s"$ord|{{ str }}|"
      case EnumLiteral(name) =>
        s"~{{ str }}~"
      case StringLiteral(str) =>
        "*\"{{ str }}\"*"
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
      case InvokeAbstractOperationExpression(name, args) =>
        s"{{ str }}({{ expr }}*)"
      case InvokeNumericMethodExpression(base, name, args) =>
        s"{{ str }}::{{ str }}({{ expr }}*)"
      case InvokeAbstractClosureExpression(x, args) =>
        s"{{ var }}({{ expr }}*)"
      case InvokeMethodExpression(base, args) =>
        s"{{ ... }}({{ expr }}*)"
      case InvokeSyntaxDirectedOperationExpression(base, name, args) =>
        if (name == "Evaluation")
          s"the result of evaluating {{ expr }}"
        else if (name == "Contains")
          s"{{ expr }} Contains {{ expr }}"
        else if (args.isEmpty)
          "{{ str }} of {{ expr }} with no arguments"
        else
          "{{ str }} of {{ expr }} with argument(s) {{ expr }}*"
      case ListExpression(Nil) =>
        s"« »"
      case ListExpression(entries) =>
        s"« {{ expr }}* »"
      case IntListExpression(from, isFromInc, to, isToInc, isInc) =>
        val from = if (isFromInc) "inclusive" else "exclusive"
        val to = if (isToInc) "inclusive" else "exclusive"
        val asc = if (isInc) "ascending" else "descending"
        s"a List of the integers in the interval from {{ expr }} ($from) to {{ expr }} ($to), in $asc order"
      case SoleElementExpression(expr) =>
        s"the sole element of {{ expr }}"
      case CodeUnitAtExpression(base, index) =>
        s"the code unit at index {{ expr }} within {{ expr }}"
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
        s"{{ expr }} is {{ ty }}*"
      case HasFieldCondition(ref, neg, field) =>
        s"{{ ref }} has a {{ field }} internal slot"
      case HasBindingCondition(ref, neg, binding) =>
        s"{{ ref }} has a binding for {{ binding }}"
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
      case InclusiveIntervalCondition(left, neg, from, to) =>
        val n = if (neg) " not" else ""
        s"{{ expr }} is${n} in the inclusive interval from {{ expr }} to {{ expr }}"
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
}

object CaseCollector {
  enum Kind { case Step, Expr, Cond, Etc }
}
