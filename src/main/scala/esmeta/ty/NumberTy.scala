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
    case (
          NumberNegIntTy | NumberPosIntTy | NumberNonNegIntTy |
          NumberNonPosIntTy | NumberIntTy,
          NumberIntTy,
        ) =>
      true
    case (NumberSetTy(set), NumberIntTy) =>
      set.forall(_.double.isWhole)
    case (NumberNegIntTy | NumberNonPosIntTy, NumberNonPosIntTy) => true
    case (NumberSetTy(set), NumberNonPosIntTy) =>
      set.forall(m => m.double.isWhole && m.double <= 0)
    case (NumberPosIntTy | NumberNonNegIntTy, NumberNonNegIntTy) => true
    case (NumberSetTy(set), NumberNonNegIntTy) =>
      set.forall(m => m.double.isWhole && m.double >= 0)
    case (NumberNegIntTy, NumberNegIntTy) => true
    case (NumberSetTy(set), NumberNegIntTy) =>
      set.forall(m => m.double.isWhole && m.double < 0)
    case (NumberPosIntTy, NumberPosIntTy) => true
    case (NumberSetTy(set), NumberPosIntTy) =>
      set.forall(m => m.double.isWhole && m.double > 0)
    case (NumberSetTy(lset), NumberSetTy(rset)) => lset subsetOf rset
    case _                                      => false

  /** union type */
  def ||(that: => NumberTy): NumberTy = (this, that) match
    case _ if this eq that                   => this
    case (NumberTopTy, _) | (_, NumberTopTy) => Top
    case (NumberIntTy, NumberSetTy(set)) =>
      if (set.exists(m => !m.double.isWhole)) Top
      else NumberIntTy
    case (NumberIntTy, _) => NumberIntTy
    case (NumberNonPosIntTy, NumberSetTy(set)) =>
      if (set.exists(m => !m.double.isWhole)) Top
      else if (set.exists(m => m.double >= 0)) NumberIntTy
      else NumberNonPosIntTy
    case (
          NumberNonPosIntTy,
          NumberPosIntTy | NumberNonNegIntTy | NumberIntTy,
        ) =>
      NumberIntTy
    case (NumberNonPosIntTy, _) => NumberNonPosIntTy
    case (NumberNonNegIntTy, NumberSetTy(set)) =>
      if (set.exists(m => !m.double.isWhole)) Top
      else if (set.exists(m => m.double < 0)) NumberIntTy
      else NumberNonNegIntTy
    case (
          NumberNonNegIntTy,
          NumberNegIntTy | NumberNonPosIntTy | NumberIntTy,
        ) =>
      NumberIntTy
    case (NumberNonNegIntTy, _) => NumberNonNegIntTy
    case (NumberNegIntTy, NumberSetTy(set)) =>
      if (set.exists(m => !m.double.isWhole)) Top
      else if (set.exists(m => m.double > 0)) NumberIntTy
      else if (set contains Number(0)) NumberNonNegIntTy
      else NumberNegIntTy
    case (NumberNegIntTy, NumberPosIntTy | NumberNonNegIntTy | NumberIntTy) =>
      NumberIntTy
    case (NumberNegIntTy, NumberNonPosIntTy) => NumberNonPosIntTy
    case (NumberNegIntTy, _)                 => NumberNegIntTy
    case (NumberPosIntTy, NumberSetTy(set)) =>
      if (set.exists(m => !m.double.isWhole)) Top
      else if (set.exists(m => m.double < 0)) NumberIntTy
      else if (set contains Number(0)) NumberNonNegIntTy
      else NumberPosIntTy
    case (NumberPosIntTy, NumberNegIntTy | NumberNonPosIntTy | NumberIntTy) =>
      NumberIntTy
    case (NumberPosIntTy, NumberNonNegIntTy)    => NumberNonNegIntTy
    case (NumberPosIntTy, _)                    => NumberPosIntTy
    case (NumberSetTy(lset), NumberSetTy(rset)) => NumberSetTy(lset union rset)
    case (NumberSetTy(set), _)                  => that || this

  /** intersection type */
  def &&(that: => NumberTy): NumberTy = (this, that) match
    case _ if this eq that => this
    case (_, NumberTopTy)  => this
    case (NumberTopTy, _)  => that
    case (NumberIntTy, NumberSetTy(set)) =>
      NumberSetTy(set.filter(_.double.isWhole))
    case (NumberIntTy, _)                       => that
    case (NumberNonPosIntTy, NumberIntTy)       => NumberNonPosIntTy
    case (NumberNonPosIntTy, NumberNonNegIntTy) => Zero
    case (NumberNonPosIntTy, NumberPosIntTy)    => Bot
    case (NumberNonPosIntTy, NumberSetTy(set)) =>
      NumberSetTy(set.filter(m => m.double.isWhole && m.double <= 0))
    case (NumberNonPosIntTy, _)              => that
    case (NumberNonNegIntTy, NumberIntTy)    => NumberNonNegIntTy
    case (NumberNonNegIntTy, NumberNegIntTy) => Zero
    case (NumberNonNegIntTy, NumberPosIntTy) => NumberPosIntTy
    case (NumberNonNegIntTy, NumberSetTy(set)) =>
      NumberSetTy(set.filter(m => m.double.isWhole && m.double >= 0))
    case (NumberNonNegIntTy, NumberNonPosIntTy)               => Zero
    case (NumberNegIntTy, NumberNonPosIntTy | NumberIntTy)    => NumberNegIntTy
    case (NumberNegIntTy, NumberNonNegIntTy | NumberPosIntTy) => Bot
    case (NumberNegIntTy, NumberSetTy(set)) =>
      NumberSetTy(set.filter(m => m.double.isWhole && m.double < 0))
    case (NumberNegIntTy, _)                                  => that
    case (NumberPosIntTy, NumberNonNegIntTy | NumberIntTy)    => NumberPosIntTy
    case (NumberPosIntTy, NumberNonPosIntTy | NumberNegIntTy) => Bot
    case (NumberPosIntTy, NumberSetTy(set)) =>
      NumberSetTy(set.filter(m => m.double.isWhole && m.double > 0))
    case (NumberPosIntTy, _) => that
    case (NumberSetTy(lset), NumberSetTy(rset)) =>
      NumberSetTy(lset intersect rset)
    case _ => that && this

  /** prune type */
  def --(that: => NumberTy): NumberTy = (this, that) match
    case _ if this eq that                => Bot
    case (_, NumberTopTy)                 => Bot
    case (NumberTopTy, _)                 => Top
    case (NumberIntTy, NumberIntTy)       => Bot
    case (NumberIntTy, NumberNonPosIntTy) => NumberPosIntTy
    case (NumberIntTy, NumberNonNegIntTy) => NumberNegIntTy
    case (NumberIntTy, NumberNegIntTy)    => NumberNonNegIntTy
    case (NumberIntTy, NumberPosIntTy)    => NumberNonPosIntTy
    case (NumberIntTy, _)                 => NumberIntTy
    case (NumberNonPosIntTy, NumberNonPosIntTy | NumberIntTy) => Bot
    case (NumberNonPosIntTy, NumberNonNegIntTy)               => NumberNegIntTy
    case (NumberNonPosIntTy, NumberNegIntTy)                  => Zero
    case (NumberNonPosIntTy, _) => NumberNonPosIntTy
    case (NumberNonNegIntTy, NumberNonNegIntTy | NumberIntTy) => Bot
    case (NumberNonNegIntTy, NumberNonPosIntTy)               => NumberPosIntTy
    case (NumberNonNegIntTy, NumberPosIntTy)                  => Zero
    case (NumberNonNegIntTy, _) => NumberNonNegIntTy
    case (NumberNegIntTy, NumberNegIntTy | NumberNonPosIntTy | NumberIntTy) =>
      Bot
    case (NumberNegIntTy, _) => NumberNegIntTy
    case (NumberPosIntTy, NumberPosIntTy | NumberNonNegIntTy | NumberIntTy) =>
      Bot
    case (NumberPosIntTy, _) => NumberPosIntTy
    case (NumberSetTy(set), NumberIntTy) =>
      NumberSetTy(set.filter(m => !m.double.isWhole))
    case (NumberSetTy(set), NumberNonPosIntTy) =>
      NumberSetTy(set.filter(m => !(m.double.isWhole && m.double <= 0)))
    case (NumberSetTy(set), NumberNonNegIntTy) =>
      NumberSetTy(set.filter(m => !(m.double.isWhole && m.double >= 0)))
    case (NumberSetTy(set), NumberNegIntTy) =>
      NumberSetTy(set.filter(m => !(m.double.isWhole && m.double < 0)))
    case (NumberSetTy(set), NumberPosIntTy) =>
      NumberSetTy(set.filter(m => !(m.double.isWhole && m.double > 0)))
    case (NumberSetTy(lset), NumberSetTy(rset)) => NumberSetTy(lset -- rset)

  /** inclusion check */
  def contains(number: Number): Boolean = this match
    case NumberTopTy       => true
    case NumberIntTy       => number.double.isWhole
    case NumberNonPosIntTy => number.double.isWhole && number.double <= 0
    case NumberNonNegIntTy => number.double.isWhole && number.double >= 0
    case NumberNegIntTy    => number.double.isWhole && number.double < 0
    case NumberPosIntTy    => number.double.isWhole && number.double > 0
    case NumberSetTy(set)  => set contains number

  /** get single value */
  def getSingle: Flat[Number] = this match
    case NumberSetTy(set) => Flat(set)
    case _                => Many

  /** integral check */
  def isInt: Boolean = this match
    case NumberIntTy      => true
    case NumberSetTy(set) => set.forall(n => n.double.isWhole)
    case _                => false
}

/** number top types */
case object NumberTopTy extends NumberTy

/** integral number types */
case object NumberIntTy extends NumberTy

/** non-positive integral number type */
case object NumberNonPosIntTy extends NumberTy

/** non-negative integral number type */
case object NumberNonNegIntTy extends NumberTy

/** negative integral number type */
case object NumberNegIntTy extends NumberTy

/** positive integral number type */
case object NumberPosIntTy extends NumberTy

/** types for set of numbers */
case class NumberSetTy(set: Set[Number]) extends NumberTy

object NumberTy extends Parser.From(Parser.numberTy) {
  lazy val Top: NumberTy = NumberTopTy
  lazy val Bot: NumberTy = NumberSetTy(Set.empty)
  lazy val Int: NumberTy = NumberIntTy
  lazy val NonPosInt: NumberTy = NumberNonPosIntTy
  lazy val NonNegInt: NumberTy = NumberNonNegIntTy
  lazy val NegInt: NumberTy = NumberNegIntTy
  lazy val PosInt: NumberTy = NumberPosIntTy
  lazy val Zero: NumberTy = NumberSetTy(Set(Number(0)))
  lazy val One: NumberTy = NumberSetTy(Set(Number(1)))
}
