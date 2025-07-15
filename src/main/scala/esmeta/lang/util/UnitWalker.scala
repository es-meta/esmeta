package esmeta.lang.util

import esmeta.util.BasicUnitWalker
import esmeta.lang.*

/** a unit walker for metalanguage */
trait UnitWalker extends BasicUnitWalker {
  def walk(elem: LangElem): Unit = elem match {
    case elem: Syntax                         => walk(elem)
    case elem: ForEachOwnPropertyKeyStepOrder => walk(elem)
    case elem: ConversionExpressionOperator   => walk(elem)
    case elem: PredicateConditionOperator     => walk(elem)
    case elem: MathFuncExpressionOperator     => walk(elem)
    case elem: BinaryExpressionOperator       => walk(elem)
    case elem: UnaryExpressionOperator        => walk(elem)
    case elem: XRefExpressionOperator         => walk(elem)
    case elem: BinaryConditionOperator        => walk(elem)
    case elem: ContainsConditionTarget        => walk(elem)
    case elem: CompoundConditionOperator      => walk(elem)
  }

  def walk(syn: Syntax): Unit = syn match {
    case syn: Block      => walk(syn)
    case syn: SubStep    => walk(syn)
    case syn: Step       => walk(syn)
    case syn: Expression => walk(syn)
    case syn: Condition  => walk(syn)
    case syn: Reference  => walk(syn)
    case syn: Type       => walk(syn)
    case syn: Property   => walk(syn)
    case syn: Intrinsic  => walk(syn)
  }

  def walk(block: Block): Unit = block match {
    case block: StepBlock => walk(block)
    case ExprBlock(exprs) => walkList(exprs, walk)
    case Figure(lines)    =>
  }

  def walk(stepBlock: StepBlock): Unit =
    walkList(stepBlock.steps, walk)

  def walk(subStep: SubStep): Unit =
    val SubStep(directive, step) = subStep
    walkOpt(directive, walk); walk(step)

  def walk(directive: Directive): Unit =
    val Directive(name, values) = directive
    walk(name); walkList(values, walk)

  def walk(step: Step): Unit = step match {
    case LetStep(x, expr) =>
      walk(x); walk(expr)
    case SetStep(x, expr) =>
      walk(x); walk(expr)
    case SetAsStep(x, verb, id) =>
      walk(x); walk(verb); walk(id)
    case SetFieldsWithIntrinsicsStep(ref, desc) =>
      walk(ref); walk(desc)
    case IfStep(cond, thenStep, elseStep) =>
      walk(cond); walk(thenStep); walkOpt(elseStep, walk)
    case ReturnStep(expr) =>
      walkOpt(expr, walk)
    case AssertStep(cond) =>
      walk(cond)
    case ForEachStep(ty, elem, expr, ascending, body) =>
      walkOpt(ty, walk); walk(elem); walk(expr); walk(body)
    case ForEachIntegerStep(x, low, high, ascending, body) =>
      walk(x); walk(low); walk(high); walk(body)
    case ForEachOwnPropertyKeyStep(key, obj, cond, ascending, order, body) =>
      walk(key); walk(obj); walk(cond); walk(body)
    case ForEachParseNodeStep(x, expr, body) =>
      walk(x); walk(expr); walk(body)
    case ThrowStep(expr)         => walk(expr)
    case PerformStep(expr)       => walk(expr)
    case PerformBlockStep(block) => walk(block)
    case AppendStep(expr, ref)   => walk(expr); walk(ref)
    case PrependStep(expr, ref)  => walk(expr); walk(ref)
    case RepeatStep(cond, body)  => walkOpt(cond, walk); walk(body)
    case PushCtxtStep(ref)       => walk(ref)
    case NoteStep(note)          =>
    case SuspendStep(base, _)    => walk(base)
    case RemoveStep(elem, list)  => walk(elem); walk(list)
    case RemoveFirstStep(expr)   => walk(expr)
    case RemoveContextStep(remove, restore) =>
      walk(remove); walkOpt(restore, walk)
    case SetEvaluationStateStep(base, func, args) =>
      walk(base); walk(func); walkList(args, walk)
    case ResumeEvaluationStep(b, aOpt, pOpt, steps) =>
      walk(b); walkOpt(aOpt, walk); walkOpt(pOpt, walk); walkList(steps, walk)
    case ResumeYieldStep(callerCtxt, arg, genCtxt, param, steps) =>
      walk(callerCtxt); walk(arg); walk(genCtxt); walk(param);
      walkList(steps, walk)
    case ReturnToResumeStep(base, retStep) =>
      walk(base); walk(retStep)
    case BlockStep(block) => walk(block)
    case YetStep(expr)    => walk(expr)
  }

  def walk(expr: Expression): Unit = expr match {
    case StringConcatExpression(exprs) =>
      walkList(exprs, walk)
    case ListConcatExpression(exprs) =>
      walkList(exprs, walk)
    case ListCopyExpression(expr) =>
      walk(expr)
    case RecordExpression(ty, fields) =>
      walk(ty); walkList(fields, { case (f, e) => walk(f); walk(e) })
    case LengthExpression(expr) =>
      walk(expr)
    case SubstringExpression(expr, from, to) =>
      walk(expr); walk(from); walkOpt(to, walk)
    case TrimExpression(expr, leading, trailing) =>
      walk(expr); walk(leading); walk(trailing)
    case NumberOfExpression(expr) =>
      walk(expr)
    case SourceTextExpression(expr) =>
      walk(expr)
    case CoveredByExpression(from, to) =>
      walk(from); walk(to)
    case GetItemsExpression(nt, expr) =>
      walk(nt); walk(expr)
    case IntrinsicExpression(intr) =>
      walk(intr)
    case expr: CalcExpression =>
      walk(expr)
    case ClampExpression(target, lower, upper) =>
      walk(target); walk(lower); walk(upper)
    case MathOpExpression(op, args) =>
      walk(op); walkList(args, walk)
    case BitwiseExpression(left, op, right) =>
      walk(left); walk(op); walk(right)
    case invoke: InvokeExpression =>
      walk(invoke)
    case ListExpression(entries) =>
      walkList(entries, walk)
    case IntListExpression(from, isFromInc, to, isToInc, isInc) =>
      walk(from); walk(to)
    case XRefExpression(kind, id) =>
      walk(kind);
    case SoleElementExpression(expr) =>
      walk(expr)
    case CodeUnitAtExpression(base, index) =>
      walk(base); walk(index)
    case multi: MultilineExpression =>
      walk(multi)
    case yet: YetExpression =>
      walk(yet)
  }

  def walk(multi: MultilineExpression): Unit = multi match {
    case AbstractClosureExpression(params, captured, body) =>
      walkList(params, walk); walkList(captured, walk); walk(body)
  }

  def walk(yet: YetExpression): Unit =
    val YetExpression(str, block) = yet
    walkOpt(block, walk)

  def walk(expr: CalcExpression): Unit = expr match {
    case ReturnIfAbruptExpression(expr, check) =>
      walk(expr)
    case ReferenceExpression(ref) =>
      walk(ref)
    case MathFuncExpression(op, args) =>
      walk(op); walkList(args, walk)
    case ConversionExpression(op, expr) =>
      walk(op); walk(expr)
    case ExponentiationExpression(base, power) =>
      walk(base); walk(power)
    case BinaryExpression(left, op, right) =>
      walk(left); walk(op); walk(right)
    case UnaryExpression(op, expr) =>
      walk(op); walk(expr)
    case lit: Literal =>
      walk(lit)
  }

  def walk(order: ForEachOwnPropertyKeyStepOrder): Unit = {}

  def walk(op: MathOpExpressionOperator): Unit = {}

  def walk(op: MathFuncExpressionOperator): Unit = {}

  def walk(op: ConversionExpressionOperator): Unit = {}

  def walk(op: BinaryExpressionOperator): Unit = {}

  def walk(op: UnaryExpressionOperator): Unit = {}

  def walk(op: BitwiseExpressionOperator): Unit = {}

  def walk(op: XRefExpressionOperator): Unit = {}

  def walk(lit: Literal): Unit = {}

  def walk(invoke: InvokeExpression): Unit = invoke match {
    case InvokeAbstractOperationExpression(name, args) =>
      walkList(args, walk)
    case InvokeNumericMethodExpression(ty, name, args) =>
      walk(ty); walkList(args, walk)
    case InvokeAbstractClosureExpression(x, args) =>
      walk(x); walkList(args, walk)
    case InvokeMethodExpression(ref, args) =>
      walk(ref); walkList(args, walk)
    case InvokeSyntaxDirectedOperationExpression(base, name, args) =>
      walk(base); walkList(args, walk)
  }

  def walk(cond: Condition): Unit = cond match {
    case ExpressionCondition(expr) =>
      walk(expr)
    case TypeCheckCondition(expr, neg, ty) =>
      walk(expr); walk(neg); walkList(ty, walk)
    case HasFieldCondition(ref, neg, field) =>
      walk(ref); walk(neg); walk(field)
    case HasBindingCondition(ref, neg, binding) =>
      walk(ref); walk(neg); walk(binding)
    case ProductionCondition(nt, lhs, rhs) =>
      walk(nt);
    case PredicateCondition(expr, neg, op) =>
      walk(expr); walk(neg); walk(op)
    case IsAreCondition(ls, neg, rs) =>
      walkList(ls, walk); walk(neg); walkList(rs, walk)
    case BinaryCondition(left, op, right) =>
      walk(left); walk(op); walk(right)
    case InclusiveIntervalCondition(left, neg, from, to) =>
      walk(left); walk(neg); walk(from); walk(to)
    case ContainsCondition(list, neg, target) =>
      walk(list); walk(neg); walk(target)
    case CompoundCondition(left, op, right) =>
      walk(left); walk(op); walk(right)
  }

  def walk(op: PredicateConditionOperator): Unit = {}

  def walk(op: BinaryConditionOperator): Unit = {}

  def walk(target: ContainsConditionTarget): Unit =
    import ContainsConditionTarget.*
    target match
      case Expr(expr) => walk(expr)
      case WhoseField(tyOpt, fieldName, expr) =>
        walkOpt(tyOpt, walk); walk(expr)
      case SuchThat(tyOpt, x, cond) =>
        walkOpt(tyOpt, walk); walk(x); walk(cond)

  def walk(op: CompoundConditionOperator): Unit = {}

  def walk(ref: Reference): Unit = ref match {
    case x: Variable                => walk(x)
    case RunningExecutionContext()  =>
    case SecondExecutionContext()   =>
    case CurrentRealmRecord()       =>
    case ActiveFunctionObject()     =>
    case propRef: PropertyReference => walk(propRef)
    case AgentRecord()              =>
  }

  def walk(x: Variable): Unit = {}

  def walk(propRef: PropertyReference): Unit = propRef match {
    case PropertyReference(base, prop) => walk(base); walk(prop)
  }

  def walk(prop: Property): Unit = prop match {
    case FieldProperty(n)        =>
    case ComponentProperty(c)    =>
    case BindingProperty(b)      => walk(b)
    case IndexProperty(e)        => walk(e)
    case IntrinsicProperty(intr) => walk(intr)
    case NonterminalProperty(n)  =>
  }

  def walk(intr: Intrinsic): Unit = {}

  def walk(ty: Type): Unit = {}
}
