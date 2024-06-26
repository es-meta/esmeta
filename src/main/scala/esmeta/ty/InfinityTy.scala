package esmeta.ty

import esmeta.util.*
import esmeta.ty.util.Parser

/** boolean types */
case class InfinityTy(pos: Set[Boolean] = Set())
  extends TyElem
  with Lattice[InfinityTy] {
  import InfinityTy.*

  /** top check */
  def isTop: Boolean = this == Top

  /** bottom check */
  def isBottom: Boolean = this == Bot

  /** partial order/subset operator */
  def <=(that: => InfinityTy): Boolean = this.pos subsetOf that.pos

  /** union type */
  def ||(that: => InfinityTy): InfinityTy = InfinityTy(this.pos ++ that.pos)

  /** intersection type */
  def &&(that: => InfinityTy): InfinityTy = InfinityTy(
    this.pos intersect that.pos,
  )

  /** prune type */
  def --(that: => InfinityTy): InfinityTy = InfinityTy(this.pos -- that.pos)

  /** inclusion check */
  def contains(b: Boolean): Boolean = pos contains b

  /** get single value */
  def getSingle: Flat[Boolean] = Flat(pos)
}
object InfinityTy extends Parser.From(Parser.infTy) {
  lazy val Top: InfinityTy = InfinityTy(Set(true, false))
  lazy val Pos: InfinityTy = InfinityTy(Set(true))
  lazy val Neg: InfinityTy = InfinityTy(Set(false))
  lazy val Bot: InfinityTy = InfinityTy()
}
