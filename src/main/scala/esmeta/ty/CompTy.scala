package esmeta.ty

import esmeta.analyzer.domain.*
import esmeta.util.*
import esmeta.state.*
import esmeta.ty.util.Parser

/** completion record types */
case class CompTy(
  normal: PureValueTy = PureValueTy.Bot,
  abrupt: Boolean = false,
) extends TyElem
  with Lattice[CompTy] {
  import CompTy.*

  /** bottom check */
  def isBottom: Boolean = (this eq Bot) | (
    this.normal.isBottom &
    !this.abrupt
  )

  /** partial order/subset operator */
  def <=(that: => CompTy): Boolean = (this eq that) | (
    this.normal <= that.normal &
    this.abrupt <= that.abrupt
  )

  /** union type */
  def |(that: => CompTy): CompTy =
    if (this eq that) this
    else
      CompTy(
        this.normal | that.normal,
        this.abrupt | that.abrupt,
      )

  /** intersection type */
  def &(that: => CompTy): CompTy =
    if (this eq that) this
    else
      CompTy(
        this.normal & that.normal,
        this.abrupt & that.abrupt,
      )

  /** prune type */
  def --(that: => CompTy): CompTy =
    if (that.isBottom) this
    else
      CompTy(
        this.normal -- that.normal,
        this.abrupt -- that.abrupt,
      )

  /** get single value */
  def getSingle: Flat[AValue] =
    if (abrupt) Many
    else normal.getSingle.map(AComp(Const("normal"), _, None))
}
object CompTy extends Parser.From(Parser.compTy) {
  val Bot: CompTy = CompTy()
}
