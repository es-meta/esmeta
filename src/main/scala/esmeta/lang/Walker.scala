package esmeta.lang

import esmeta.util.BasicWalker

/** a walker for metalanguage */
trait Walker extends BasicWalker {
  def walk(elem: LangElem): LangElem = elem match {
    case elem: Program => walk(elem)
    case elem: Block   => walk(elem)
    case elem: Step    => walk(elem)
  }

  def walk(prog: Program): Program = Program(walk(prog.block))

  def walk(block: Block): Block = block match {
    case Order(steps)   => Order(walkList(steps, walk))
    case Unorder(steps) => Unorder(walkList(steps, walk))
    case Figure(lines)  => Figure(lines)
  }

  def walk(step: Step): Step = step match {
    case Yet(str, block) => Yet(str, walkOpt(block, walk))
  }
}
