package esmeta.domain

import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*

/** flat abstraction */
enum Flat[+T] {
  case Many extends Flat[Nothing]
  case One(value: T) extends Flat[T]
  case Zero extends Flat[Nothing]

  /** top element check */
  def isTop: Boolean = this == Many

  /** bottom element check */
  final inline def nonTop: Boolean = !this.isTop

  /** bottom element check */
  def isBottom: Boolean = this == Zero

  /** bottom element check */
  final inline def nonBottom: Boolean = !this.isBottom

  /** partial order */
  def ⊑[U >: T](that: Flat[U]): Boolean = (this, that) match
    case (Zero, _) | (_, Many) => true
    case (Many, _) | (_, Zero) => false
    case (One(l), One(r))      => l == r

  /** partial order */
  final inline def <=[U >: T](that: Flat[U]): Boolean = this ⊑ that

  /** not partial order */
  final inline def !⊑[U >: T](that: Flat[U]): Boolean = !(this ⊑ that)

  /** not partial order */
  final inline def !<=[U >: T](that: Flat[U]): Boolean = !(this ⊑ that)

  /** join operator */
  def ⊔[U >: T](that: Flat[U]): Flat[U] = (this, that) match
    case (Many, _) | (_, Many) => Many
    case (Zero, _)             => that
    case (_, Zero)             => this
    case (One(l), One(r))      => if (l == r) this else Many

  /** join operator */
  final inline def ||[U >: T](that: Flat[U]): Flat[U] = this ⊔ that

  /** meet operator */
  def ⊓[U >: T](that: Flat[U]): Flat[U] = (this, that) match
    case (Zero, _) | (_, Zero) => Zero
    case (Many, _)             => that
    case (_, Many)             => this
    case (One(l), One(r))      => if (l == r) this else Zero

  /** meet operator */
  final inline def &&[U >: T](that: Flat[U]): Flat[U] = this ⊓ that

  /** check inclusion */
  def contains[U >: T](value: U): Boolean = this match
    case Many   => true
    case One(v) => v == value
    case Zero   => false

  /** concretization to set domain */
  def toBSet: BSet[T] = this match
    case Many       => BSet.Inf
    case One(value) => BSet(value)
    case Zero       => BSet()

  /** concretization to flat domain */
  def toFlat: Flat[T] = this

  /** map function */
  def map[U](f: T => U): Flat[U] = this match
    case Many       => Many
    case One(value) => One(f(value))
    case Zero       => Zero

  /** map function */
  def to[E](f: T => E)(using lattice: Lattice[E]): E = this match
    case Many       => lattice.Top
    case One(value) => f(value)
    case Zero       => lattice.Bot

  /** prune operator */
  def --[U >: T](that: Flat[U]): Flat[U] = (this, that) match
    case (Many, One(_))        => Many
    case (_, Zero)             => this
    case (One(l), One(r))      => if (l == r) Zero else this
    case (Zero, _) | (_, Many) => Zero

  /** element addition */
  def +[U >: T](x: U): Flat[U] = this match
    case One(y) if x == y => this
    case Zero             => One(x)
    case _                => Many

  override def toString: String = this match
    case Many       => "⊤"
    case One(value) => value.toString
    case Zero       => "⊥"
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

given [T: Rule]: Rule[Flat[T]] = (app, elem) =>
  import Flat.*
  elem match {
    case Many       => app >> "⊤"
    case One(value) => app >> value
    case Zero       => app >> "⊥"
  }
