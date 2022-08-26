package esmeta.analyzer.domain

import esmeta.analyzer.*

/** bounded set lattice */
sealed trait BSet[+A] {

  /** join operator */
  def ⊔[U >: A](that: => BSet[U]): BSet[U] = (this, that) match
    case (Inf, _) | (_, Inf)    => Inf
    case (Fin(lset), Fin(rset)) => Fin(lset ++ rset)

  /** map function */
  def map[B](f: A => B): BSet[B] = this match
    case Fin(set) => Fin(set.map(f))
    case Inf      => exploded(s"impossible to iterate infinite set.")
}
case object Inf extends BSet[Nothing]
case class Fin[A](set: Set[A]) extends BSet[A]
object Fin:
  def apply[A](elems: A*): Fin[A] = Fin(elems.toSet)
