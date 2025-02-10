package esmeta.util.domain

import esmeta.util.BaseUtils.*
import Flat.*, BSet.*

/** flat abstraction */
enum Flat[+T] {
  case Many extends Flat[Nothing]
  case One(value: T) extends Flat[T]
  case Zero extends Flat[Nothing]

  /** map function */
  def map[U](f: T => U): Flat[U] = this match
    case Many       => Many
    case One(value) => One(f(value))
    case Zero       => Zero

  /** prune operator */
  def --[U >: T](that: Flat[U]): Flat[U] = (this, that) match
    case (Many, One(_))        => Many
    case (_, Zero)             => this
    case (One(l), One(r))      => if (l == r) Zero else this
    case (Zero, _) | (_, Many) => Zero

  def isTop: Boolean = this == Many
  def isBottom: Boolean = this == Zero
  def <=[U >: T](that: Flat[U]): Boolean = (this, that) match
    case (Zero, _) | (_, Many) => true
    case (Many, _) | (_, Zero) => false
    case (One(l), One(r))      => l == r
  def ||[U >: T](that: Flat[U]): Flat[U] = (this, that) match
    case (Many, _) | (_, Many) => Many
    case (Zero, _)             => that
    case (_, Zero)             => this
    case (One(l), One(r))      => if (l == r) this else Many
  def &&[U >: T](that: Flat[U]): Flat[U] = (this, that) match
    case (Zero, _) | (_, Zero) => Zero
    case (Many, _)             => that
    case (_, Many)             => this
    case (One(l), One(r))      => if (l == r) this else Zero
  def contains[U >: T](value: U): Boolean = this match
    case Many   => true
    case One(v) => v == value
    case Zero   => false
  def toBSet: BSet[T] = this match
    case Many       => Inf
    case One(value) => Fin(Set(value))
    case Zero       => Fin(Set())
  def toFlat: Flat[T] = this
}
object Flat {
  val Top = Many
  val Bot = Zero
  inline def apply[T](elems: T*): Flat[T] = apply(elems)
  def apply[T](elems: Iterable[T]): Flat[T] =
    if (elems.isEmpty) Zero
    else if (elems.size == 1) One(elems.head)
    else Many
}
