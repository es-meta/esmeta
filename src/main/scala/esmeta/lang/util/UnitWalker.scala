package esmeta.lang.util

import esmeta.util.BasicUnitWalker
import esmeta.lang.*

/** a unit walker for metalanguage */
trait UnitWalker extends BasicUnitWalker {
  def walk(elem: LangElem): Unit = elem match {
    case elem: Syntax                => walk(elem)
    case elem: PredicateCondition.Op => walk(elem)
    case elem: MathOpExpression.Op   => walk(elem)
    case elem: BinaryExpression.Op   => walk(elem)
    case elem: UnaryExpression.Op    => walk(elem)
    case elem: XRefExpression.Op     => walk(elem)
    case elem: BinaryCondition.Op    => walk(elem)
    case elem: CompoundCondition.Op  => walk(elem)
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
    val SubStep(idTag, step) = subStep
    walk(step)

  def walk(step: Step): Unit = step match {
    case LetStep(x, expr) =>
      walk(x); walk(expr)
    case SetStep(x, expr) =>
      walk(x); walk(expr)
    case IfStep(cond, thenStep, elseStep) =>
      walk(cond); walk(thenStep); walkOpt(elseStep, walk)
    case ReturnStep(expr) =>
      walkOpt(expr, walk)
    case AssertStep(cond) =>
      walk(cond)
    case ForEachStep(ty, elem, expr, ascending, body) =>
      walkOpt(ty, walk); walk(elem); walk(expr); walk(body)
    case ForEachIntegerStep(x, start, cond, ascending, body) =>
      walk(x); walk(start); walk(cond); walk(body)
    case ForEachArrayIndexStep(key, array, start, ascending, body) =>
      walk(key); walk(array); walk(start); walk(body)
    case ThrowStep(errorName)   =>
    case PerformStep(expr)      => walk(expr)
    case AppendStep(expr, ref)  => walk(expr); walk(ref)
    case RepeatStep(cond, body) => walkOpt(cond, walk); walk(body)
    case PushCtxtStep(ref)      => walk(ref)
    case NoteStep(note)         =>
    case SuspendStep(base, _)   => walk(base)
    case SetEvaluationStateStep(base, p, body) =>
      walk(base); walkOpt(p, walk); walk(body)
    case ResumeEvaluationStep(b, aOpt, pOpt, steps) =>
      walk(b); walkOpt(aOpt, walk); walkOpt(pOpt, walk); walkList(steps, walk)
    case ReturnToResumeStep(base, arg) =>
      walk(base); walk(arg)
    case BlockStep(block) => walk(block)
    case YetStep(expr)    => walk(expr)
  }

  def walk(expr: Expression): Unit = expr match {
    case StringConcatExpression(exprs) =>
      walkList(exprs, walk)
    case ListConcatExpression(exprs) =>
      walkList(exprs, walk)
    case RecordExpression(ty, fields) =>
      walk(ty); walkList(fields, { case (f, e) => walk(f); walk(e) })
    case LengthExpression(expr) =>
      walk(expr)
    case SubstringExpression(expr, from, to) =>
      walk(expr); walk(from); walk(to)
    case NumberOfExpression(expr) =>
      walk(expr)
    case SourceTextExpression(expr) =>
      walk(expr)
    case CoveredByExpression(from, to) =>
      walk(from); walk(to)
    case IntrinsicExpression(intr) =>
      walk(intr)
    case expr: CalcExpression =>
      walk(expr)
    case invoke: InvokeExpression =>
      walk(invoke)
    case ListExpression(entries) =>
      walkList(entries, walk)
    case XRefExpression(kind, id) =>
      walk(kind);
    case SoleElementExpression(expr) =>
      walk(expr)
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
    case MathOpExpression(op, args) =>
      walk(op); walkList(args, walk)
    case ExponentiationExpression(base, power) =>
      walk(base); walk(power)
    case BinaryExpression(left, op, right) =>
      walk(left); walk(op); walk(right)
    case UnaryExpression(op, expr) =>
      walk(op); walk(expr)
    case lit: Literal =>
      walk(lit)
  }

  def walk(op: MathOpExpression.Op): Unit = {}

  def walk(op: BinaryExpression.Op): Unit = {}

  def walk(op: UnaryExpression.Op): Unit = {}

  def walk(op: XRefExpression.Op): Unit = {}

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
    case InstanceOfCondition(expr, neg, ty) =>
      walk(expr); walk(neg); walkList(ty, walk)
    case HasFieldCondition(ref, neg, field) =>
      walk(ref); walk(neg); walk(field)
    case ProductionCondition(nt, lhs, rhs) =>
      walk(nt);
    case PredicateCondition(expr, neg, op) =>
      walk(expr); walk(neg); walk(op)
    case IsAreCondition(ls, neg, rs) =>
      walkList(ls, walk); walk(neg); walkList(rs, walk)
    case BinaryCondition(left, op, right) =>
      walk(left); walk(op); walk(right)
    case CompoundCondition(left, op, right) =>
      walk(left); walk(op); walk(right)
  }

  def walk(op: BinaryCondition.Op): Unit = {}

  def walk(op: CompoundCondition.Op): Unit = {}

  def walk(op: PredicateCondition.Op): Unit = {}

  def walk(ref: Reference): Unit = ref match {
    case PropertyReference(base, prop) => walk(base); walk(prop)
    case _                             =>
  }

  def walk(prop: Property): Unit = prop match {
    case IndexProperty(e)        => walk(e)
    case IntrinsicProperty(intr) => walk(intr)
    case _                       =>
  }

  def walk(intr: Intrinsic): Unit = {}

  def walk(ty: Type): Unit = {}
}
