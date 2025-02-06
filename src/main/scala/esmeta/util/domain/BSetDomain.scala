package esmeta.util.domain

import esmeta.util.BaseUtils.*
import scala.annotation.unchecked.uncheckedVariance
import Flat.*, BSet.*

/** set abstract domains */
trait BSetDomain[A] extends Domain[A, BSet[A]]:
  val Top = Inf
  val Bot = Fin(Set())
  def alpha(elems: Iterable[A]): BSet[A] = BSet(elems)

enum BSet[+A]:
  case Inf extends BSet[Nothing]
  case Fin(set: Set[A @uncheckedVariance]) extends BSet[A]

object BSet:
  val Top = Inf
  val Bot = BSet()
  inline def apply[A](elems: A*): BSet[A] = apply(elems)
  inline def apply[A](elems: Iterable[A]): BSet[A] = Fin(elems.toSet)

given bsetOps[A]: Ops[A, BSet[A]] with
  extension (elem: BSet[A])
    def isTop: Boolean = elem == Inf
    def isBottom: Boolean = elem == Fin(Set())
    def ⊑(that: BSet[A]): Boolean = (elem, that) match
      case (_, Inf)               => true
      case (Inf, _)               => false
      case (Fin(lset), Fin(rset)) => lset subsetOf rset
    def ⊔(that: BSet[A]): BSet[A] = (elem, that) match
      case (Inf, _) | (_, Inf)    => Inf
      case (Fin(lset), Fin(rset)) => Fin(lset ++ rset)
    def ⊓(that: BSet[A]): BSet[A] = (elem, that) match
      case (Inf, _)               => that
      case (_, Inf)               => elem
      case (Fin(lset), Fin(rset)) => Fin(lset intersect rset)
    def contains(value: A): Boolean = elem match
      case Inf      => true
      case Fin(set) => set contains value
    def gamma: BSet[A] = elem
    def getSingle: Flat[A] = elem match
      case Inf                       => Many
      case Fin(set) if set.size == 1 => One(set.head)
      case Fin(_)                    => Zero

    /** map function */
    def map[B](f: A => B): BSet[B] = elem match
      case Inf      => Inf
      case Fin(set) => Fin(set map f)

    /** prune operator */
    def --(that: BSet[A]): BSet[A] = (elem, that) match
      case (_, Inf)         => Fin(Set())
      case (Inf, _)         => elem
      case (Fin(l), Fin(r)) => Fin(l -- r)

    /** unsound conversion to list */
    def unsoundList: List[A] = elem match
      case Fin(set) => set.toList
      case Inf      => warn(s"unsound iteration on infinite set"); Nil
