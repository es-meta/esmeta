package esmeta.lang.util

import esmeta.util.BasicWalker
import esmeta.lang.*

/** a walker for metalanguage */
trait Walker extends BasicWalker {
  def walk(elem: LangElem): LangElem = elem match {
    case elem: Syntax               => walk(elem)
    case elem: MathOpExpression.Op  => walk(elem)
    case elem: BinaryExpression.Op  => walk(elem)
    case elem: UnaryExpression.Op   => walk(elem)
    case elem: XRefExpression.Op    => walk(elem)
    case elem: BinaryCondition.Op   => walk(elem)
    case elem: CompoundCondition.Op => walk(elem)
  }

  def walk(syn: Syntax): Syntax = syn match {
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

  def walk(block: Block): Block = block match {
    case block: StepBlock => walk(block)
    case ExprBlock(exprs) => ExprBlock(walkList(exprs, walk))
    case Figure(lines)    => Figure(lines)
  }

  def walk(stepBlock: StepBlock): StepBlock =
    StepBlock(walkList(stepBlock.steps, walk))

  def walk(subStep: SubStep): SubStep =
    val SubStep(idTag, step) = subStep
    SubStep(idTag, walk(step))

  def walk(step: Step): Step = step match {
    case LetStep(x, expr) =>
      LetStep(walk(x), walk(expr))
    case SetStep(x, expr) =>
      SetStep(walk(x), walk(expr))
    case IfStep(cond, thenStep, elseStep) =>
      IfStep(walk(cond), walk(thenStep), walkOpt(elseStep, walk))
    case ReturnStep(expr) =>
      ReturnStep(walkOpt(expr, walk))
    case AssertStep(cond) =>
      AssertStep(walk(cond))
    case ForEachStep(ty, elem, expr, ascending, body) =>
      ForEachStep(
        walkOpt(ty, walk),
        walk(elem),
        walk(expr),
        ascending,
        walk(body),
      )
    case ForEachIntegerStep(x, start, cond, ascending, body) =>
      ForEachIntegerStep(
        walk(x),
        walk(start),
        walk(cond),
        ascending,
        walk(body),
      )
    case ThrowStep(errorName)   => ThrowStep(errorName)
    case PerformStep(expr)      => PerformStep(walk(expr))
    case AppendStep(expr, ref)  => AppendStep(walk(expr), walk(ref))
    case RepeatStep(cond, body) => RepeatStep(walkOpt(cond, walk), walk(body))
    case PushCtxtStep(ref)      => PushCtxtStep(walk(ref))
    case NoteStep(note)         => NoteStep(note)
    case SuspendStep(base, r)   => SuspendStep(walk(base), r)
    case BlockStep(block)       => BlockStep(walk(block))
    case YetStep(expr)          => YetStep(walk(expr))
  }

  def walk(expr: Expression): Expression = expr match {
    case StringConcatExpression(exprs) =>
      StringConcatExpression(walkList(exprs, walk))
    case ListConcatExpression(exprs) =>
      ListConcatExpression(walkList(exprs, walk))
    case RecordExpression(ty, fields) =>
      lazy val newFields =
        walkList(fields, { case (f, e) => (walk(f), walk(e)) })
      RecordExpression(walk(ty), newFields)
    case LengthExpression(expr) =>
      LengthExpression(walk(expr))
    case SubstringExpression(expr, from, to) =>
      SubstringExpression(walk(expr), walk(from), walk(to))
    case NumberOfExpression(expr) =>
      NumberOfExpression(walk(expr))
    case SourceTextExpression(expr) =>
      SourceTextExpression(walk(expr))
    case CoveredByExpression(code, rule) =>
      CoveredByExpression(walk(code), walk(rule))
    case IntrinsicExpression(intr) =>
      IntrinsicExpression(walk(intr))
    case expr: CalcExpression =>
      walk(expr)
    case invoke: InvokeExpression =>
      walk(invoke)
    case ReturnIfAbruptExpression(expr, check) =>
      ReturnIfAbruptExpression(walk(expr), check)
    case ListExpression(entries) =>
      ListExpression(walkList(entries, walk))
    case XRefExpression(kind, id) =>
      XRefExpression(walk(kind), id)
    case multi: MultilineExpression => walk(multi)
    case yet: YetExpression =>
      walk(yet)
  }

  def walk(multi: MultilineExpression): MultilineExpression = multi match {
    case AbstractClosureExpression(params, captured, body) =>
      AbstractClosureExpression(
        walkList(params, walk),
        walkList(captured, walk),
        walk(body),
      )
  }

  def walk(yet: YetExpression): YetExpression =
    val YetExpression(str, block) = yet
    YetExpression(str, walkOpt(block, walk))

  def walk(expr: CalcExpression): CalcExpression = expr match {
    case ReferenceExpression(ref) =>
      ReferenceExpression(walk(ref))
    case lit: Literal =>
      walk(lit)
    case MathOpExpression(op, args) =>
      MathOpExpression(walk(op), walkList(args, walk))
    case ExponentiationExpression(base, power) =>
      ExponentiationExpression(walk(base), walk(power))
    case BinaryExpression(left, op, right) =>
      BinaryExpression(walk(left), walk(op), walk(right))
    case UnaryExpression(op, expr) =>
      UnaryExpression(walk(op), walk(expr))
  }

  def walk(op: MathOpExpression.Op): MathOpExpression.Op = op

  def walk(op: BinaryExpression.Op): BinaryExpression.Op = op

  def walk(op: UnaryExpression.Op): UnaryExpression.Op = op

  def walk(op: XRefExpression.Op): XRefExpression.Op = op

  def walk(lit: Literal): Literal = lit
  def walk(flit: FieldLiteral): FieldLiteral = flit

  def walk(invoke: InvokeExpression): InvokeExpression = invoke match {
    case InvokeAbstractOperationExpression(name, args) =>
      InvokeAbstractOperationExpression(name, walkList(args, walk))
    case InvokeNumericMethodExpression(ty, name, args) =>
      InvokeNumericMethodExpression(walk(ty), name, walkList(args, walk))
    case InvokeAbstractClosureExpression(x, args) =>
      InvokeAbstractClosureExpression(walk(x), walkList(args, walk))
    case InvokeMethodExpression(ref, args) =>
      InvokeMethodExpression(walk(ref), walkList(args, walk))
    case InvokeSyntaxDirectedOperationExpression(base, name, args) =>
      InvokeSyntaxDirectedOperationExpression(
        walk(base),
        name,
        walkList(args, walk),
      )
  }

  def walk(cond: Condition): Condition = cond match {
    case ExpressionCondition(expr) =>
      ExpressionCondition(walk(expr))
    case InstanceOfCondition(expr, neg, ty) =>
      InstanceOfCondition(walk(expr), walk(neg), walkList(ty, walk))
    case HasFieldCondition(ref, neg, field) =>
      HasFieldCondition(walk(ref), walk(neg), walk(field))
    case AbruptCompletionCondition(x, neg) =>
      AbruptCompletionCondition(walk(x), walk(neg))
    case ProductionCondition(nt, lhs, rhs) =>
      ProductionCondition(walk(nt), lhs, rhs)
    case IsAreCondition(ls, neg, rs) =>
      IsAreCondition(walkList(ls, walk), walk(neg), walkList(rs, walk))
    case BinaryCondition(left, op, right) =>
      BinaryCondition(walk(left), walk(op), walk(right))
    case CompoundCondition(left, op, right) =>
      CompoundCondition(walk(left), walk(op), walk(right))
  }

  def walk(op: BinaryCondition.Op): BinaryCondition.Op = op

  def walk(op: CompoundCondition.Op): CompoundCondition.Op = op

  def walk(ref: Reference): Reference = ref match {
    case x: Variable                => walk(x)
    case propRef: PropertyReference => walk(propRef)
    case _                          => ref
  }

  def walk(propRef: PropertyReference): PropertyReference = propRef match {
    case PropertyReference(base, prop) =>
      PropertyReference(walk(base), walk(prop))
  }

  def walk(x: Variable): Variable = Variable(x.name)

  def walk(prop: Property): Property = prop match {
    case FieldProperty(n)        => FieldProperty(n)
    case IndexProperty(e)        => IndexProperty(walk(e))
    case IntrinsicProperty(intr) => IntrinsicProperty(walk(intr))
    case ComponentProperty(c)    => ComponentProperty(c)
  }

  def walk(intr: Intrinsic): Intrinsic = intr

  def walk(ty: Type): Type = Type(ty.name)
}
