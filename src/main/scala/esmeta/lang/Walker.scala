package esmeta.lang

import esmeta.util.BasicWalker

/** a walker for metalanguage */
trait Walker extends BasicWalker {
  def walk(elem: LangElem): LangElem = elem match {
    case elem: Program    => walk(elem)
    case elem: Block      => walk(elem)
    case elem: Step       => walk(elem)
    case elem: Expression => walk(elem)
    case elem: Identifier => walk(elem)
  }

  def walk(prog: Program): Program = Program(walk(prog.block))

  def walk(block: Block): Block = block match {
    case Order(steps)   => Order(walkList(steps, walk))
    case Unorder(steps) => Unorder(walkList(steps, walk))
    case Figure(lines)  => Figure(lines)
  }

  def walk(step: Step): Step = step match {
    case LetStep(x, expr)    => LetStep(walk(x), walk(expr))
    case YetStep(str, block) => YetStep(str, walkOpt(block, walk))
  }

  def walk(expr: Expression): Expression = expr match {
    case LengthExpression(expr)   => LengthExpression(walk(expr))
    case IdentifierExpression(id) => IdentifierExpression(walk(id))
  }

  def walk(id: Identifier): Identifier = id match {
    case x: Variable => walk(x)
  }

  def walk(x: Variable): Variable = Variable(x.name)
}
