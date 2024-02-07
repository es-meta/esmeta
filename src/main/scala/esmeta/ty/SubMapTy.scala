package esmeta.ty

import esmeta.util.*
import esmeta.state.Value
import esmeta.ty.util.Parser

/** sub map types */
case class SubMapTy(
  key: PureValueTy = PureValueTy.Bot,
  value: PureValueTy = PureValueTy.Bot,
) extends TyElem
  with Lattice[SubMapTy] {
  import SubMapTy.*

  /** top check */
  def isTop: Boolean = (this eq Top) || (
    this.key.isTop &&
    this.value.isTop
  )

  /** bottom check */
  def isBottom: Boolean = (this eq Bot) || (
    this.key.isBottom &&
    this.value.isBottom
  )

  /** partial order/subset operator */
  def <=(that: => SubMapTy): Boolean = (this eq that) || (
    this.key <= that.key &&
    this.value <= that.value
  )

  /** union type */
  def ||(that: => SubMapTy): SubMapTy =
    if (this eq that) this
    else
      SubMapTy(
        this.key || that.key,
        this.value || that.value,
      )

  /** intersection type */
  def &&(that: => SubMapTy): SubMapTy =
    if (this eq that) this
    else
      SubMapTy(
        this.key && that.key,
        this.value && that.value,
      ).norm

  /** prune type */
  def --(that: => SubMapTy): SubMapTy =
    if (that.isBottom) this
    else
      SubMapTy(
        this.key -- that.key,
        this.value -- that.value,
      ).norm

  /** get single value */
  def getSingle: Flat[Value] = if (this.isBottom) Zero else Many

  // normalization
  private def norm: SubMapTy =
    if (key.isBottom || value.isBottom) Bot
    else this
}
object SubMapTy extends Parser.From(Parser.subMapTy) {
  val Top = SubMapTy(PureValueTy.Top, PureValueTy.Top)
  val Bot: SubMapTy = SubMapTy()
}
