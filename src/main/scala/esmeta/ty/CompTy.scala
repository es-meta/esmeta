package esmeta.ty

import esmeta.analyzer.domain.*
import esmeta.util.*
import esmeta.state.*
import esmeta.ty.util.Parser

/** completion record types */
case class CompTy(
  normal: Option[PureValueTy] = Some(PureValueTy.Bot),
  abrupt: Boolean = false,
) extends TyElem
  with Lattice[CompTy] {
  import CompTy.*

  /** bottom check */
  def isBottom: Boolean = (this eq Bot) || (
    this.normal.fold(false)(_.isBottom) &&
    !this.abrupt
  )

  /** partial order/subset operator */
  def <=(that: => CompTy): Boolean = (this eq that) || (
    ((this.normal, that.normal) match
      case (_, None)          => true
      case (None, _)          => false
      case (Some(l), Some(r)) => l <= r
    ) && this.abrupt <= that.abrupt
  )

  /** union type */
  def ||(that: => CompTy): CompTy =
    if (this eq that) this
    else
      CompTy(
        (this.normal, that.normal) match
          case (None, _) | (_, None) => None
          case (Some(l), Some(r))    => Some(l || r)
        ,
        this.abrupt || that.abrupt,
      )

  /** intersection type */
  def &&(that: => CompTy): CompTy =
    if (this eq that) this
    else
      CompTy(
        (this.normal, that.normal) match
          case (None, r)          => r
          case (l, None)          => l
          case (Some(l), Some(r)) => Some(l && r)
        ,
        this.abrupt && that.abrupt,
      )

  /** prune type */
  def --(that: => CompTy): CompTy =
    if (that.isBottom) this
    else
      CompTy(
        (this.normal, that.normal) match
          case (None, _)          => None
          case (_, None)          => Some(PureValueTy.Bot)
          case (Some(l), Some(r)) => Some(l -- r)
        ,
        this.abrupt -- that.abrupt,
      )

  /** get single value */
  def getSingle: Flat[AValue] =
    if (abrupt) Many
    else normal.fold(Many)(_.getSingle.map(AComp(Const("normal"), _, None)))
}
object CompTy extends Parser.From(Parser.compTy) {
  val Bot: CompTy = CompTy()
  val Top = CompTy(None, true)
}
