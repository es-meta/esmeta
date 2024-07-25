package esmeta.ty

import esmeta.util.*
import esmeta.state.Value
import esmeta.ty.util.Parser

/** map types */
case class MapTy(
  key: PureValueTy = PureValueTy.Bot,
  value: PureValueTy = PureValueTy.Bot,
) extends TyElem
  with Lattice[MapTy] {
  import MapTy.*

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
  def <=(that: => MapTy): Boolean = (this eq that) || (
    this.key <= that.key &&
    this.value <= that.value
  )

  /** union type */
  def ||(that: => MapTy): MapTy =
    if (this eq that) this
    else
      MapTy(
        this.key || that.key,
        this.value || that.value,
      )

  /** intersection type */
  def &&(that: => MapTy): MapTy =
    if (this eq that) this
    else
      MapTy(
        this.key && that.key,
        this.value && that.value,
      ).normalized

  /** prune type */
  def --(that: => MapTy): MapTy =
    if (that.isBottom) this
    else
      MapTy(
        this.key -- that.key,
        this.value -- that.value,
      ).normalized

  /** get single value */
  def getSingle: Flat[Value] = if (this.isBottom) Zero else Many

  /** normalized type */
  def normalized: MapTy =
    if (key.isBottom || value.isBottom) Bot
    else this
}
object MapTy extends Parser.From(Parser.mapTy) {
  val Top = MapTy(PureValueTy.Top, PureValueTy.Top)
  val Bot: MapTy = MapTy()
}
