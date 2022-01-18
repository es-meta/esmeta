package esmeta.lang

import esmeta.util.BasicWalker

/** a walker for metalanguage */
trait Walker extends BasicWalker {
  def walk(elem: LangElem): LangElem = elem match {
    case elem: Program              => walk(elem)
    case elem: Block                => walk(elem)
    case elem: SubStep              => walk(elem)
    case elem: Step                 => walk(elem)
    case elem: Expression           => walk(elem)
    case elem: Condition            => walk(elem)
    case elem: Reference            => walk(elem)
    case elem: BinaryExpression.Op  => walk(elem)
    case elem: UnaryExpression.Op   => walk(elem)
    case elem: BinaryCondition.Op   => walk(elem)
    case elem: CompoundCondition.Op => walk(elem)
  }

  def walk(prog: Program): Program = Program(walk(prog.block))

  def walk(block: Block): Block = block match {
    case StepBlock(steps) => StepBlock(walkList(steps, walk))
    case ExprBlock(exprs) => ExprBlock(walkList(exprs, walk))
    case Figure(lines)    => Figure(lines)
  }

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
      ReturnStep(walk(expr))
    case AssertStep(cond) =>
      AssertStep(walk(cond))
    case ForEachIntegerStep(x, start, cond, ascending, body) =>
      ForEachIntegerStep(
        walk(x),
        walk(start),
        walk(cond),
        ascending,
        walk(body),
      )
    case ThrowStep(errorName) => ThrowStep(errorName)
    case PerformStep(expr)    => PerformStep(walk(expr))
    case BlockStep(block)     => BlockStep(walk(block))
    case YetStep(expr)        => YetStep(walk(expr))
  }

  def walk(expr: Expression): Expression = expr match {
    case LengthExpression(expr) =>
      LengthExpression(walk(expr))
    case SubstringExpression(expr, from, to) =>
      SubstringExpression(walk(expr), walk(from), walk(to))
    case expr: CalcExpression =>
      walk(expr)
    case invoke: InvokeExpression =>
      walk(invoke)
    case ReturnIfAbruptExpression(expr, check) =>
      ReturnIfAbruptExpression(walk(expr), check)
    case ListExpression(entries) =>
      ListExpression(walkList(entries, walk))
    case NonterminalExpression(name) =>
      NonterminalExpression(name)
    case yet: YetExpression =>
      walk(yet)
  }

  def walk(yet: YetExpression): YetExpression =
    val YetExpression(str, block) = yet
    YetExpression(str, walkOpt(block, walk))

  def walk(expr: CalcExpression): CalcExpression = expr match {
    case ReferenceExpression(ref) =>
      ReferenceExpression(walk(ref))
    case lit: Literal =>
      walk(lit)
    case BinaryExpression(left, op, right) =>
      BinaryExpression(walk(left), walk(op), walk(right))
    case UnaryExpression(op, expr) =>
      UnaryExpression(walk(op), walk(expr))
  }

  def walk(invoke: InvokeExpression): InvokeExpression = invoke match {
    case InvokeAbstractOperationExpression(name, args) =>
      InvokeAbstractOperationExpression(name, walkList(args, walk))
    case InvokeSyntaxDirectedOperationExpression(base, name, args) =>
      InvokeSyntaxDirectedOperationExpression(
        walk(base),
        name,
        walkList(args, walk),
      )
  }

  def walk(op: BinaryExpression.Op): BinaryExpression.Op = op

  def walk(op: UnaryExpression.Op): UnaryExpression.Op = op

  def walk(lit: Literal): Literal = lit

  def walk(cond: Condition): Condition = cond match {
    case ExpressionCondition(expr) =>
      ExpressionCondition(walk(expr))
    case HasFieldCondition(expr, fieldName) =>
      HasFieldCondition(walk(expr), fieldName)
    case BinaryCondition(left, op, right) =>
      BinaryCondition(walk(left), walk(op), walk(right))
    case CompoundCondition(left, op, right) =>
      CompoundCondition(walk(left), walk(op), walk(right))
  }

  def walk(op: BinaryCondition.Op): BinaryCondition.Op = op

  def walk(op: CompoundCondition.Op): CompoundCondition.Op = op

  def walk(ref: Reference): Reference = ref match {
    case Field(base, name) => Field(walk(base), name)
    case x: Variable       => walk(x)
  }

  def walk(x: Variable): Variable = Variable(x.name)
}
