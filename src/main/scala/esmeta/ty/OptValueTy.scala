package esmeta.ty

import esmeta.state.*
import esmeta.util.*
import esmeta.ty.util.Parser

/** optinoal value types */
case class OptValueTy(
  value: ValueTy,
  optional: Boolean = false,
) extends TyElem
  with Lattice[OptValueTy] {

  import OptValueTy.*
  import ManualInfo.tyModel.{isSubTy, getBase}

  /** top check */
  def isTop: Boolean = value.isTop && optional.isTop

  /** bottom check */
  def isBottom: Boolean = value.isBottom && optional.isBottom

  /** partial order/subset operator */
  def <=(that: => OptValueTy): Boolean = (this eq that) || {
    this.value <= that.value &&
    this.optional <= that.optional
  }

  /** union type */
  def ||(that: => OptValueTy): OptValueTy =
    if (this eq that) this
    else
      OptValueTy(
        this.value || that.value,
        this.optional || that.optional,
      )

  /** intersection type */
  def &&(that: => OptValueTy): OptValueTy =
    if (this eq that) this
    else
      OptValueTy(
        this.value && that.value,
        this.optional && that.optional,
      )

  /** prune type */
  def --(that: => OptValueTy): OptValueTy =
    if (that.isBottom) this
    else
      OptValueTy(
        this.value -- that.value,
        this.optional -- that.optional,
      )

  /** optional value containment check */
  def contains(optValue: Option[Value], heap: Heap): Boolean = optValue match
    case Some(value) => this.value.contains(value, heap)
    case None        => this.optional
}
object OptValueTy extends Parser.From(Parser.optValueTy) {
  lazy val Top: OptValueTy = OptValueTy(ValueTy.Top, true)
  lazy val Empty: OptValueTy = OptValueTy(ValueTy.Bot, true)
  lazy val Exist: OptValueTy = OptValueTy(ValueTy.Top, false)
  lazy val Bot: OptValueTy = OptValueTy(ValueTy.Bot, false)
}
