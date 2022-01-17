package esmeta.lang

import esmeta.util.BasicUnitWalker

/** a unit walker for metalanguage */
trait UnitWalker extends BasicUnitWalker {
  def walk(elem: LangElem): Unit = elem match {
    case elem: Program    => walk(elem)
    case elem: Block      => walk(elem)
    case elem: Step       => walk(elem)
    case elem: Expression => walk(elem)
    case elem: Identifier => walk(elem)
  }

  def walk(prog: Program): Unit = walk(prog.block)

  def walk(block: Block): Unit = block match {
    case Order(steps)   => walkList(steps, walk)
    case Unorder(steps) => walkList(steps, walk)
    case Figure(lines)  =>
  }

  def walk(step: Step): Unit = step match {
    case LetStep(x, expr) =>
      walk(x); walk(expr)
    case IfStep(cond, thenStep, elseStep) =>
      walk(cond); walk(thenStep); walkOpt(elseStep, walk)
    case ReturnStep(expr) =>
      walk(expr)
    case AssertStep(cond) =>
      walk(cond)
    case YetStep(str, block) =>
      walkOpt(block, walk)
  }

  def walk(expr: Expression): Unit = expr match {
    case LengthExpression(expr)   => walk(expr)
    case IdentifierExpression(id) => walk(id)
    case lit: Literal             => walk(lit)
  }

  def walk(lit: Literal): Unit = {}

  def walk(cond: Condition): Unit = cond match {
    case ExpressionCondition(expr) =>
      walk(expr)
    case BinaryCondition(left, op, right) =>
      walk(left); walk(op); walk(right)
    case CompoundCondition(left, op, right) =>
      walk(left); walk(op); walk(right)
  }

  def walk(op: BinaryOp): Unit = {}

  def walk(op: CompoundOp): Unit = {}

  def walk(id: Identifier): Unit = id match {
    case x: Variable => walk(x)
  }

  def walk(x: Variable): Unit = {}
}
