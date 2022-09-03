package esmeta.ty

import esmeta.analyzer.domain.*
import esmeta.util.*
import esmeta.ty.util.Parser

/** sub map types */
case class SubMapTy(
  key: PureValueTy = PureValueTy(),
  value: PureValueTy = PureValueTy(),
) extends TyElem
  with Lattice[SubMapTy] {

  /** bottom check */
  def isBottom: Boolean =
    this.key.isBottom &
    this.value.isBottom

  /** partial order/subset operator */
  def <=(that: => SubMapTy): Boolean =
    this.key <= that.key &
    this.value <= that.value

  /** union type */
  def |(that: => SubMapTy): SubMapTy = SubMapTy(
    this.key | that.key,
    this.value | that.value,
  )

  /** intersection type */
  def &(that: => SubMapTy): SubMapTy = SubMapTy(
    this.key & that.key,
    this.value & that.value,
  ).norm

  /** prune type */
  def --(that: => SubMapTy): SubMapTy = SubMapTy(
    this.key -- that.key,
    this.value -- that.value,
  ).norm

  /** get single value */
  def getSingle: Flat[AValue] = if (this.isBottom) Zero else Many

  // normalization
  private def norm: SubMapTy =
    if (key.isBottom | value.isBottom) SubMapTy()
    else this
}
object SubMapTy extends Parser.From(Parser.subMapTy)
