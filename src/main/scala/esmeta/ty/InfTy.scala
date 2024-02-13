package esmeta.ty

import esmeta.util.*
import esmeta.ty.util.Parser

/** boolean types */
case class InfTy(isPos: Set[Boolean] = Set())
  extends TyElem
  with Lattice[InfTy] {
  import InfTy.*

  /** top check */
  def isTop: Boolean = this == Top

  /** bottom check */
  def isBottom: Boolean = this == Bot

  /** partial order/subset operator */
  def <=(that: => InfTy): Boolean = this.isPos subsetOf that.isPos

  /** union type */
  def ||(that: => InfTy): InfTy = InfTy(this.isPos ++ that.isPos)

  /** intersection type */
  def &&(that: => InfTy): InfTy = InfTy(this.isPos intersect that.isPos)

  /** prune type */
  def --(that: => InfTy): InfTy = InfTy(this.isPos -- that.isPos)
}
object InfTy extends Parser.From(Parser.infTy) {
  lazy val Top: InfTy = InfTy(Set(true, false))
  lazy val Pos: InfTy = InfTy(Set(true))
  lazy val Neg: InfTy = InfTy(Set(false))
  lazy val Bot: InfTy = InfTy()
}
