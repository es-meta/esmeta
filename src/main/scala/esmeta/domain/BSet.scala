package esmeta.domain

import esmeta.util.BaseUtils.*
import esmeta.util.Appender.*
import scala.annotation.unchecked.uncheckedVariance

enum BSet[+T] {
  case Inf extends BSet[Nothing]
  case Fin(set: Set[T @uncheckedVariance]) extends BSet[T]

  /** top element check */
  def isTop: Boolean = this == Inf

  /** bottom element check */
  def isBottom: Boolean = this == Fin(Set())

  /** partial order */
  def ⊑[U >: T](that: BSet[U]): Boolean = (this, that) match
    case (_, Inf)               => true
    case (Inf, _)               => false
    case (Fin(lset), Fin(rset)) => lset.toSet subsetOf rset.toSet

  /** partial order */
  final inline def <=[U >: T](that: BSet[U]): Boolean = this ⊑ that

  /** not partial order */
  final inline def !⊑[U >: T](that: BSet[U]): Boolean = !(this ⊑ that)

  /** not partial order */
  final inline def !<=[U >: T](that: BSet[U]): Boolean = !(this ⊑ that)

  /** join operator */
  def ⊔[U >: T](that: BSet[U]): BSet[U] = (this, that) match
    case (Inf, _) | (_, Inf)    => Inf
    case (Fin(lset), Fin(rset)) => Fin(lset ++ rset)

  /** join operator */
  final inline def ||[U >: T](that: BSet[U]): BSet[U] = this ⊔ that

  /** meet operator */
  def ⊓[U >: T](that: BSet[U]): BSet[U] = (this, that) match
    case (Inf, _)               => that
    case (_, Inf)               => this
    case (Fin(lset), Fin(rset)) => Fin(lset.toSet intersect rset.toSet)

  /** meet operator */
  final inline def &&[U >: T](that: BSet[U]): BSet[U] = this ⊓ that

  /** check inclusion */
  def contains[U >: T](value: U): Boolean = this match
    case Inf      => true
    case Fin(set) => set.toSet contains value

  /** concretization to set domain */
  def toBSet: BSet[T] = this

  /** concretization to flat domain */
  def toFlat: Flat[T] = this match
    case Inf                       => Flat.Many
    case Fin(set) if set.size == 1 => Flat.One(set.head)
    case Fin(_)                    => Flat.Zero

  /** map function */
  def map[U](f: T => U): BSet[U] = this match
    case Inf      => Inf
    case Fin(set) => Fin(set map f)

  /** conversion function */
  def to[E: Lattice.Ops](f: T => E)(using lattice: Lattice[E]): E = this match
    case Inf      => lattice.Top
    case Fin(set) => set.foldLeft[E](lattice.Bot) { _ ⊔ f(_) }

  /** prune operator */
  def --[U >: T](that: BSet[U]): BSet[U] = (this, that) match
    case (_, Inf)         => Fin(Set())
    case (Inf, _)         => this
    case (Fin(l), Fin(r)) => Fin(l.toSet -- r.toSet)

  /** unsound conversion to list */
  def unsoundList: List[T] = this match
    case Fin(set) => set.toList
    case Inf      => warn(s"unsound iteration on infinite set"); Nil

  /** element addition */
  def +[U >: T](x: U): BSet[U] = this match
    case Inf      => Inf
    case Fin(set) => Fin(set.toSet + x)

  /** element addition */
  def ++[U >: T](xs: Iterable[U]): BSet[U] = this match
    case Inf      => Inf
    case Fin(set) => Fin(set ++ xs)

  override def toString: String = this match
    case Inf => "⊤"
    case Fin(set) =>
      if (set.isEmpty) "⊥"
      else if (set.size == 1) set.head.toString
      else set.mkString("{", ", ", "}")
}

object BSet {
  val Top = Inf
  val Bot = Fin(Set())
  inline def apply[T](elems: T*): BSet[T] = apply(elems)
  inline def apply[T](elems: Iterable[T]): BSet[T] = Fin(elems.toSet)
}

import BSet.*

given [T: Rule]: Rule[BSet[T]] = bsetRule()

/** bounded set appender */
def bsetRule[V: Rule](
  left: String = "{",
  sep: String = ",",
  right: String = "}",
): Rule[BSet[V]] = (app, bset) =>
  bset match
    case Inf => app >> "⊤"
    case Fin(set) =>
      given Rule[Set[V]] = iterableRule(left, sep, right)
      if (set.isEmpty) app >> "⊥"
      if (set.size == 1) app >> set.head
      else app >> set

/** sorted bounded set appender */
def sortedBSetRule[V: Ordering: Rule](
  left: String = "{",
  sep: String = ",",
  right: String = "}",
): Rule[BSet[V]] = (app, bset) =>
  bset match
    case Inf => app >> "⊤"
    case Fin(set) =>
      given Rule[Set[V]] = sortedSetRule(left, sep, right)
      if (set.isEmpty) app >> "⊥"
      if (set.size == 1) app >> set.head
      else app >> set
