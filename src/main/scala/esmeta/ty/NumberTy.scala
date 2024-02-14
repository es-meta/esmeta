package esmeta.ty

import esmeta.state.*
import esmeta.ty.util.Parser
import esmeta.util.*

/** number types */
sealed trait NumberTy extends TyElem with Lattice[NumberTy] {
  import NumberTy.*

  /** top check */
  def isTop: Boolean = this eq Top

  /** bottom check */
  def isBottom: Boolean = this == Bot

  /** partial order/subset operator */
  def <=(that: => NumberTy): Boolean = (this, that) match
    case _ if (this eq that) || (this == Bot) => true
    case (_, NumberTopTy)                     => true
    case (NumberIntTy, NumberIntTy)           => true
    case (NumberSetTy(set), NumberIntTy) => set.forall(n => n.double.isWhole)
    case (NumberSetTy(lset), NumberSetTy(rset)) => lset subsetOf rset
    case _                                      => false

  /** union type */
  def ||(that: => NumberTy): NumberTy = (this, that) match
    case _ if this eq that                   => this
    case (NumberTopTy, _) | (_, NumberTopTy) => Top
    case (NumberIntTy, NumberIntTy)          => NumberIntTy
    case (NumberIntTy, NumberSetTy(set)) =>
      if (set.exists(n => n.double.isWhole)) Top
      else NumberIntTy
    case (NumberSetTy(lset), NumberSetTy(rset)) => NumberSetTy(lset union rset)
    case (NumberSetTy(set), _)                  => that || this

  /** intersection type */
  def &&(that: => NumberTy): NumberTy = (this, that) match
    case _ if this eq that          => this
    case (_, NumberTopTy)           => this
    case (NumberTopTy, _)           => that
    case (NumberIntTy, NumberIntTy) => that
    case (NumberIntTy, NumberSetTy(set)) =>
      NumberSetTy(set.filter(n => n.double.isWhole))
    case (NumberSetTy(lset), NumberSetTy(rset)) =>
      NumberSetTy(lset intersect rset)
    case _ => that && this

  /** prune type */
  def --(that: => NumberTy): NumberTy = (this, that) match
    case _ if this eq that          => Bot
    case (_, NumberTopTy)           => Bot
    case (NumberTopTy, _)           => Top
    case (NumberIntTy, NumberIntTy) => Bot
    case (NumberIntTy, _)           => NumberIntTy
    case (NumberSetTy(set), NumberIntTy) =>
      NumberSetTy(set.filter(n => !n.double.isWhole))
    case (NumberSetTy(lset), NumberSetTy(rset)) =>
      NumberSetTy(lset -- rset)

  /** inclusion check */
  def contains(number: Number): Boolean = this match
    case NumberTopTy      => true
    case NumberIntTy      => number.double.isWhole
    case NumberSetTy(set) => set contains number

  /** get single value */
  def getSingle: Flat[Number] = this match
    case NumberSetTy(set) => Flat(set)
    case _                => Many
}

/** number top types */
case object NumberTopTy extends NumberTy

/** integral number types */
case object NumberIntTy extends NumberTy

/** types for set of numbers */
case class NumberSetTy(set: Set[Number]) extends NumberTy

object NumberTy extends Parser.From(Parser.numberTy) {
  lazy val Top: NumberTy = NumberTopTy
  lazy val Bot: NumberTy = NumberSetTy(Set.empty)
  lazy val Int: NumberTy = NumberIntTy
}
