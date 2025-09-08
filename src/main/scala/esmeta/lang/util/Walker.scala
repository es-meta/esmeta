package esmeta.lang.util

import esmeta.util.BasicWalker
import esmeta.lang.*

/** a walker for metalanguage */
trait Walker extends BasicWalker {
  def walk(elem: LangElem): LangElem = elem match {
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
    val SubStep(directive, step) = subStep
    SubStep(walkOpt(directive, walk), walk(step))

  def walk(directive: Directive): Directive =
    val Directive(name, values) = directive
    Directive(walk(name), walkList(values, walk))

  def walk(step: Step): Step = step match {
    case LetStep(x, expr)       => LetStep(walk(x), walk(expr))
    case SetStep(x, expr)       => SetStep(walk(x), walk(expr))
    case SetAsStep(x, verb, id) => SetAsStep(walk(x), walk(verb), walk(id))
    case SetEvaluationStateStep(base, func, args) =>
      SetEvaluationStateStep(walk(base), walk(func), walkList(args, walk))
    case PerformStep(expr)          => PerformStep(walk(expr))
    case InvokeShorthandStep(x, a)  => InvokeShorthandStep(x, walkList(a, walk))
    case AppendStep(expr, ref)      => AppendStep(walk(expr), walk(ref))
    case PrependStep(expr, ref)     => PrependStep(walk(expr), walk(ref))
    case AddStep(expr, ref)         => AddStep(walk(expr), walk(ref))
    case RemoveStep(t, p, l)        => RemoveStep(walk(t), walk(p), walk(l))
    case PushContextStep(ref)       => PushContextStep(walk(ref))
    case SuspendStep(ref, rm)       => SuspendStep(walkOpt(ref, walk), walk(rm))
    case RemoveContextStep(ctxt, t) => RemoveContextStep(walk(ctxt), walk(t))
    case AssertStep(cond)           => AssertStep(walk(cond))
    case IfStep(cond, thenStep, elseStep, config) =>
      IfStep(walk(cond), walk(thenStep), walkOpt(elseStep, walk), walk(config))
    case RepeatStep(cond, body) => RepeatStep(walk(cond), walk(body))
    case ForEachStep(ty, elem, expr, forward, body) =>
      ForEachStep(
        walkOpt(ty, walk),
        walk(elem),
        walk(expr),
        forward,
        walk(body),
      )
    case ForEachIntegerStep(x, low, lowInc, high, highInc, ascending, body) =>
      ForEachIntegerStep(
        walk(x),
        walk(low),
        walk(lowInc),
        walk(high),
        walk(highInc),
        ascending,
        walk(body),
      )
    case ForEachOwnPropertyKeyStep(key, obj, cond, ascending, order, body) =>
      ForEachOwnPropertyKeyStep(
        walk(key),
        walk(obj),
        walk(cond),
        ascending,
        walk(order),
        walk(body),
      )
    case ForEachParseNodeStep(x, expr, body) =>
      ForEachParseNodeStep(
        walk(x),
        walk(expr),
        walk(body),
      )
    case ReturnStep(expr) => ReturnStep(walk(expr))
    case ThrowStep(expr)  => ThrowStep(walk(expr))
    case ResumeStep(callerCtxt, arg, genCtxt, param, steps) =>
      ResumeStep(
        walk(callerCtxt),
        walk(arg),
        walk(genCtxt),
        walk(param),
        walkList(steps, walk),
      )
    case ResumeEvaluationStep(b, aOpt, pOpt, steps) =>
      ResumeEvaluationStep(
        walk(b),
        walkOpt(aOpt, walk),
        walkOpt(pOpt, walkPair(_, walk, walk)),
        walkList(steps, walk),
      )
    case ResumeTopContextStep() => ResumeTopContextStep()
    case NoteStep(note)         => NoteStep(note)
    case BlockStep(block)       => BlockStep(walk(block))
    case YetStep(expr)          => YetStep(walk(expr))
    // -------------------------------------------------------------------------
    // special steps rarely used in the spec
    // -------------------------------------------------------------------------
    case SetFieldsWithIntrinsicsStep(ref, desc) =>
      SetFieldsWithIntrinsicsStep(walk(ref), walk(desc))
    case PerformBlockStep(b, d) =>
      PerformBlockStep(walk(b), walk(d))
  }

  def walk(target: RemoveStep.Target): RemoveStep.Target =
    import RemoveStep.Target.*
    target match
      case First(count)  => First(walkOpt(count, walk))
      case Last(count)   => Last(walkOpt(count, walk))
      case Element(elem) => Element(walk(elem))

  def walk(
    target: RemoveContextStep.RestoreTarget,
  ): RemoveContextStep.RestoreTarget =
    import RemoveContextStep.RestoreTarget.*
    target match
      case NoRestore    => NoRestore
      case StackTop     => StackTop
      case Context(ref) => Context(walk(ref))

  def walk(
    cond: RepeatStep.LoopCondition,
  ): RepeatStep.LoopCondition =
    import RepeatStep.LoopCondition.*
    cond match
      case NoCondition => NoCondition
      case While(cond) => While(walk(cond))
      case Until(cond) => Until(walk(cond))

  def walk(config: IfStep.ElseConfig): IfStep.ElseConfig =
    val IfStep.ElseConfig(newLine, keyword, comma) = config
    IfStep.ElseConfig(walk(newLine), walk(keyword), walk(comma))

  def walk(expr: Expression): Expression = expr match {
    case StringConcatExpression(exprs) =>
      StringConcatExpression(walkList(exprs, walk))
    case ListConcatExpression(exprs) =>
      ListConcatExpression(walkList(exprs, walk))
    case ListCopyExpression(expr) =>
      ListCopyExpression(walk(expr))
    case RecordExpression(ty, fields) =>
      lazy val newFields =
        walkList(fields, { case (f, e) => (walk(f), walk(e)) })
      RecordExpression(walk(ty), newFields)
    case LengthExpression(expr) =>
      LengthExpression(walk(expr))
    case SubstringExpression(expr, from, to) =>
      SubstringExpression(walk(expr), walk(from), walkOpt(to, walk))
    case TrimExpression(expr, leading, trailing) =>
      TrimExpression(walk(expr), walk(leading), walk(trailing))
    case NumberOfExpression(name, pre, expr) =>
      NumberOfExpression(walk(name), walkOpt(pre, walk), walk(expr))
    case SourceTextExpression(expr) =>
      SourceTextExpression(walk(expr))
    case CoveredByExpression(code, rule) =>
      CoveredByExpression(walk(code), walk(rule))
    case GetItemsExpression(nt, expr) =>
      GetItemsExpression(walk(nt), walk(expr))
    case IntrinsicExpression(intr) =>
      IntrinsicExpression(walk(intr))
    case expr: CalcExpression =>
      walk(expr)
    case ClampExpression(target, lower, upper) =>
      ClampExpression(walk(target), walk(lower), walk(upper))
    case MathOpExpression(op, args) =>
      MathOpExpression(walk(op), walkList(args, walk))
    case BitwiseExpression(left, op, right) =>
      BitwiseExpression(walk(left), walk(op), walk(right))
    case invoke: InvokeExpression =>
      walk(invoke)
    case ListExpression(entries, verbose) =>
      ListExpression(walkList(entries, walk), verbose)
    case IntListExpression(from, isFromInc, to, isToInc, isInc) =>
      IntListExpression(walk(from), isFromInc, walk(to), isToInc, isInc)
    case XRefExpression(kind, id) =>
      XRefExpression(walk(kind), id)
    case SoleElementExpression(expr) =>
      SoleElementExpression(walk(expr))
    case CodeUnitAtExpression(base, index) =>
      CodeUnitAtExpression(walk(base), walk(index))
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
    case ReturnIfAbruptExpression(expr, check) =>
      ReturnIfAbruptExpression(walk(expr), check)
    case ReferenceExpression(ref) =>
      ReferenceExpression(walk(ref))
    case lit: Literal =>
      walk(lit)
    case MathFuncExpression(op, args) =>
      MathFuncExpression(walk(op), walkList(args, walk))
    case ConversionExpression(op, expr) =>
      ConversionExpression(walk(op), walk(expr))
    case ExponentiationExpression(base, power) =>
      ExponentiationExpression(walk(base), walk(power))
    case BinaryExpression(left, op, right) =>
      BinaryExpression(walk(left), walk(op), walk(right))
    case UnaryExpression(op, expr) =>
      UnaryExpression(walk(op), walk(expr))
  }

  def walk(
    order: ForEachOwnPropertyKeyStepOrder,
  ): ForEachOwnPropertyKeyStepOrder = order

  def walk(op: MathOpExpressionOperator): MathOpExpressionOperator = op

  def walk(op: MathFuncExpressionOperator): MathFuncExpressionOperator = op

  def walk(op: ConversionExpressionOperator): ConversionExpressionOperator = op

  def walk(op: BinaryExpressionOperator): BinaryExpressionOperator = op

  def walk(op: UnaryExpressionOperator): UnaryExpressionOperator = op

  def walk(op: BitwiseExpressionOperator): BitwiseExpressionOperator = op

  def walk(op: XRefExpressionOperator): XRefExpressionOperator = op

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
    case TypeCheckCondition(expr, neg, ty) =>
      TypeCheckCondition(walk(expr), walk(neg), walkList(ty, walk))
    case HasFieldCondition(ref, neg, field) =>
      HasFieldCondition(walk(ref), walk(neg), walk(field))
    case HasBindingCondition(ref, neg, binding) =>
      HasBindingCondition(walk(ref), walk(neg), walk(binding))
    case ProductionCondition(nt, lhs, rhs) =>
      ProductionCondition(walk(nt), lhs, rhs)
    case PredicateCondition(expr, neg, op) =>
      PredicateCondition(walk(expr), neg, walk(op))
    case IsAreCondition(ls, neg, rs) =>
      IsAreCondition(walkList(ls, walk), walk(neg), walkList(rs, walk))
    case BinaryCondition(left, op, right) =>
      BinaryCondition(walk(left), walk(op), walk(right))
    case InclusiveIntervalCondition(left, neg, from, to) =>
      InclusiveIntervalCondition(walk(left), walk(neg), walk(from), walk(to))
    case ContainsCondition(list, neg, target) =>
      ContainsCondition(walk(list), walk(neg), walk(target))
    case CompoundCondition(left, op, right) =>
      CompoundCondition(walk(left), walk(op), walk(right))
  }

  def walk(op: PredicateConditionOperator): PredicateConditionOperator = op

  def walk(op: BinaryConditionOperator): BinaryConditionOperator = op

  def walk(target: ContainsConditionTarget): ContainsConditionTarget =
    import ContainsConditionTarget.*
    target match
      case Expr(expr) => Expr(walk(expr))
      case WhoseField(tyOpt, fieldName, expr) =>
        WhoseField(walkOpt(tyOpt, walk), fieldName, walk(expr))
      case SuchThat(tyOpt, x, cond) =>
        SuchThat(walkOpt(tyOpt, walk), walk(x), walk(cond))

  def walk(op: CompoundConditionOperator): CompoundConditionOperator = op

  def walk(ref: Reference): Reference = ref match {
    case x: Variable                => walk(x)
    case RunningExecutionContext()  => RunningExecutionContext()
    case SecondExecutionContext()   => SecondExecutionContext()
    case CurrentRealmRecord()       => CurrentRealmRecord()
    case ActiveFunctionObject()     => ActiveFunctionObject()
    case propRef: PropertyReference => walk(propRef)
    case AgentRecord()              => AgentRecord()
  }

  def walk(x: Variable): Variable = Variable(x.name)

  def walk(propRef: PropertyReference): PropertyReference = propRef match {
    case PropertyReference(base, prop) =>
      PropertyReference(walk(base), walk(prop))
  }

  def walk(prop: Property): Property = prop match {
    case FieldProperty(n)        => FieldProperty(n)
    case ComponentProperty(c)    => ComponentProperty(c)
    case BindingProperty(b)      => BindingProperty(walk(b))
    case IndexProperty(e)        => IndexProperty(walk(e))
    case IntrinsicProperty(intr) => IntrinsicProperty(walk(intr))
    case NonterminalProperty(n)  => NonterminalProperty(n)
  }

  def walk(intr: Intrinsic): Intrinsic = intr

  def walk(ty: Type): Type = Type(ty.ty)
}
