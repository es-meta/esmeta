package esmeta.lang

import esmeta.util.BasicUnitWalker

/** a unit walker for metalanguage */
trait UnitWalker extends BasicUnitWalker {
  def walk(elem: LangElem): Unit = elem match {
    case elem: Program              => walk(elem)
    case elem: Block                => walk(elem)
    case elem: SubStep              => walk(elem)
    case elem: Step                 => walk(elem)
    case elem: Expression           => walk(elem)
    case elem: Condition            => walk(elem)
    case elem: Reference            => walk(elem)
    case elem: Type                 => walk(elem)
    case elem: Field                => walk(elem)
    case elem: Property             => walk(elem)
    case elem: Intrinsic            => walk(elem)
    case elem: MathOpExpression.Op  => walk(elem)
    case elem: BinaryExpression.Op  => walk(elem)
    case elem: UnaryExpression.Op   => walk(elem)
    case elem: BinaryCondition.Op   => walk(elem)
    case elem: CompoundCondition.Op => walk(elem)
  }

  def walk(prog: Program): Unit = walk(prog.block)

  def walk(block: Block): Unit = block match {
    case StepBlock(steps) => walkList(steps, walk)
    case ExprBlock(exprs) => walkList(exprs, walk)
    case Figure(lines)    =>
  }

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
    case ForEachStep(ty, elem, expr, body) =>
      walkOpt(ty, walk); walk(elem); walk(expr); walk(body)
    case ForEachIntegerStep(x, start, cond, ascending, body) =>
      walk(x); walk(start); walk(cond); walk(body)
    case ThrowStep(errorName)   =>
    case PerformStep(expr)      => walk(expr)
    case AppendStep(expr, ref)  => walk(expr); walk(ref)
    case RepeatStep(cond, body) => walkOpt(cond, walk); walk(body)
    case PushStep(context)      => walk(context)
    case NoteStep(note)         =>
    case SuspendStep(base)      => walk(base)
    case BlockStep(block)       => walk(block)
    case YetStep(expr)          => walk(expr)
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
    case SourceTextExpression(expr) =>
      walk(expr)
    case IntrinsicExpression(intr) =>
      walk(intr)
    case expr: CalcExpression =>
      walk(expr)
    case invoke: InvokeExpression =>
      walk(invoke)
    case ReturnIfAbruptExpression(expr, check) =>
      walk(expr)
    case ListExpression(entries) =>
      walkList(entries, walk)
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
      walk(expr); walk(ty)
    case HasFieldCondition(expr, neg, field) =>
      walk(expr); walk(field)
    case AbruptCompletionCondition(x, neg) =>
      walk(x)
    case PresentCondition(expr, neg) =>
      walk(expr);
    case BinaryCondition(left, op, right) =>
      walk(left); walk(op); walk(right)
    case CompoundCondition(left, op, right) =>
      walk(left); walk(op); walk(right)
  }

  def walk(op: BinaryCondition.Op): Unit = {}

  def walk(op: CompoundCondition.Op): Unit = {}

  def walk(ref: Reference): Unit = ref match {
    case PropertyReference(base, prop) => walk(base); walk(prop)
    case _                             =>
  }

  def walk(prop: Property): Unit = prop match {
    case FieldProperty(f) => walk(f)
    case IndexProperty(e) => walk(e)
    case _                =>
  }

  def walk(field: Field): Unit = field match {
    case StringField(name)         =>
    case IntrinsicField(intrinsic) => walk(intrinsic)
  }

  def walk(intr: Intrinsic): Unit = {}

  def walk(ty: Type): Unit = {}
}
