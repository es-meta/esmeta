package esmeta.lang

import esmeta.util.BasicUnitWalker

import Block.*, Step.*

/** a unit walker for metalanguage */
trait UnitWalker extends BasicUnitWalker {
  def walk(elem: LangElem): Unit = elem match {
    case elem: Program => walk(elem)
    case elem: Block   => walk(elem)
    case elem: Step    => walk(elem)
  }

  def walk(prog: Program): Unit = walk(prog.block)

  def walk(block: Block): Unit = block match {
    case Order(steps)   => walkList(steps, walk)
    case Unorder(steps) => walkList(steps, walk)
    case Figure(lines)  =>
  }

  def walk(step: Step): Unit = step match {
    case Yet(str, block) => walkOpt(block, walk)
  }
}
