package esmeta.ty

import esmeta.util.*
import esmeta.ty.util.Parser

/** list types */
case class ListTy(elem: Option[ValueTy] = None)
  extends TyElem
  with Lattice[ListTy] {
  import ListTy.*

  /** bottom check */
  def isBottom: Boolean = (this eq Bot) | (elem == None)

  /** partial order/subset operator */
  def <=(that: => ListTy): Boolean = (this eq that) | (
    (this.elem, that.elem) match
      case (None, _)          => true
      case (_, None)          => false
      case (Some(l), Some(r)) => l <= r
  )

  /** union type */
  def |(that: => ListTy): ListTy =
    if (this eq that) this
    else
      (this.elem, that.elem) match
        case (None, _)          => that
        case (_, None)          => this
        case (Some(l), Some(r)) => ListTy(Some(l | r))

  /** intersection type */
  def &(that: => ListTy): ListTy =
    if (this eq that) this
    else
      (this.elem, that.elem) match
        case (None, _) | (_, None) => Bot
        case (Some(l), Some(r))    => ListTy(Some(l & r))

  /** prune type */
  def --(that: => ListTy): ListTy =
    if (that.isBottom) this
    else
      (this.elem, that.elem) match
        case (None, _)          => Bot
        case (_, None)          => this
        case (Some(l), Some(r)) => ListTy(Some(l -- r))

  /** get single value */
  def getSingle: Flat[Nothing] = if (elem.isEmpty) Zero else Many
}
object ListTy extends Parser.From(Parser.listTy) {
  val Bot: ListTy = ListTy()
}
