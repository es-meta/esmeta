package esmeta.ty

import esmeta.util.*

/** list types */
case class ListTy(elem: Option[ValueTy] = None)
  extends TyElem
  with Lattice[ListTy] {

  /** bottom check */
  def isBottom: Boolean = elem == None

  /** partial order/subset operator */
  def <=(that: => ListTy): Boolean = (this.elem, that.elem) match
    case (None, _)          => true
    case (_, None)          => false
    case (Some(l), Some(r)) => l <= r

  /** union type */
  def |(that: => ListTy): ListTy = (this.elem, that.elem) match
    case (None, _)          => that
    case (_, None)          => this
    case (Some(l), Some(r)) => ListTy(Some(l | r))

  /** intersection type */
  def &(that: => ListTy): ListTy = (this.elem, that.elem) match
    case (None, _) | (_, None) => ListTy()
    case (Some(l), Some(r))    => ListTy(Some(l & r))

  /** prune type */
  def --(that: => ListTy): ListTy = (this.elem, that.elem) match
    case (None, _)          => ListTy()
    case (_, None)          => this
    case (Some(l), Some(r)) => ListTy(Some(l -- r))
}
