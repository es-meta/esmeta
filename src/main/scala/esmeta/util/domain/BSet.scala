package esmeta.util.domain

import esmeta.util.BaseUtils.*
import scala.annotation.unchecked.uncheckedVariance
import Flat.*, BSet.*

enum BSet[+T] {
  case Inf extends BSet[Nothing]
  case Fin(set: Set[T @uncheckedVariance]) extends BSet[T]

  /** map function */
  def map[U](f: T => U): BSet[U] = this match
    case Inf      => Inf
    case Fin(set) => Fin(set map f)

  /** prune operator */
  def --[U >: T](that: BSet[U]): BSet[U] = (this, that) match
    case (_, Inf)         => Fin(Set())
    case (Inf, _)         => this
    case (Fin(l), Fin(r)) => Fin(l.toSet -- r.toSet)

  /** unsound conversion to list */
  def unsoundList: List[T] = this match
    case Fin(set) => set.toList
    case Inf      => warn(s"unsound iteration on infinite set"); Nil

  def isTop: Boolean = this == Inf
  def isBottom: Boolean = this == Fin(Set())
  def <=[U >: T](that: BSet[U]): Boolean = (this, that) match
    case (_, Inf)               => true
    case (Inf, _)               => false
    case (Fin(lset), Fin(rset)) => lset.toSet subsetOf rset.toSet
  def ||[U >: T](that: BSet[U]): BSet[U] = (this, that) match
    case (Inf, _) | (_, Inf)    => Inf
    case (Fin(lset), Fin(rset)) => Fin(lset ++ rset)
  def &&[U >: T](that: BSet[U]): BSet[U] = (this, that) match
    case (Inf, _)               => that
    case (_, Inf)               => this
    case (Fin(lset), Fin(rset)) => Fin(lset.toSet intersect rset.toSet)
  def contains[U >: T](value: U): Boolean = this match
    case Inf      => true
    case Fin(set) => set.toSet contains value
  def toBSet: BSet[T] = this
  def toFlat: Flat[T] = this match
    case Inf                       => Many
    case Fin(set) if set.size == 1 => One(set.head)
    case Fin(_)                    => Zero
}
object BSet {
  val Top = Inf
  val Bot = BSet()
  inline def apply[T](elems: T*): BSet[T] = apply(elems)
  inline def apply[T](elems: Iterable[T]): BSet[T] = Fin(elems.toSet)
}
