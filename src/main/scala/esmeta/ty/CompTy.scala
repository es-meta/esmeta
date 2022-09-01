package esmeta.ty

import esmeta.util.*
import esmeta.ty.util.Parser

/** completion record types */
case class CompTy(
  normal: PureValueTy = PureValueTy(),
  abrupt: Boolean = false,
) extends TyElem
  with Lattice[CompTy] {

  /** bottom check */
  def isBottom: Boolean =
    this.normal.isBottom &
    !this.abrupt

  /** partial order/subset operator */
  def <=(that: => CompTy): Boolean =
    this.normal <= that.normal &
    this.abrupt <= that.abrupt

  /** union type */
  def |(that: => CompTy): CompTy = CompTy(
    this.normal | that.normal,
    this.abrupt | that.abrupt,
  )

  /** intersection type */
  def &(that: => CompTy): CompTy = CompTy(
    this.normal & that.normal,
    this.abrupt & that.abrupt,
  )

  /** prune type */
  def --(that: => CompTy): CompTy = CompTy(
    this.normal -- that.normal,
    this.abrupt -- that.abrupt,
  )
}
object CompTy extends Parser.From(Parser.compTy)
