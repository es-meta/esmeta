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
    case _ if (this eq that) || (this == Bot)            => true
    case (_, NumberTopTy)                                => true
    case (NumberIntTy(lnan), NumberIntTy(rnan))          => !lnan || rnan
    case (NumberSubIntTy(_, _, lnan), NumberIntTy(rnan)) => !lnan || rnan
    case (NumberSetTy(lset), NumberIntTy(rnan)) =>
      lset.forall(m => m.double.isWhole || (rnan && m.double.isNaN))
    case (
          NumberSubIntTy(lpos, lzero, lnan),
          NumberSubIntTy(rpos, rzero, rnan),
        ) =>
      (lpos == rpos) && (!lzero || rzero) && (!lnan || rnan)
    case (NumberSetTy(lset), NumberSubIntTy(rpos, rzero, rnan)) =>
      lset.forall(m => {
        (rpos && m.double > 0) || (!rpos && m.double < 0) ||
        (rzero && m.double == 0) || (rnan && m.double.isNaN)
      })
    case (NumberSetTy(lset), NumberSetTy(rset)) => lset subsetOf rset
    case _                                      => false

  /** union type */
  def ||(that: => NumberTy): NumberTy = (this, that) match
    case _ if this eq that                      => this
    case (NumberTopTy, _) | (_, NumberTopTy)    => NumberTopTy
    case (NumberIntTy(lnan), NumberIntTy(rnan)) => NumberIntTy(lnan || rnan)
    case (NumberIntTy(lnan), NumberSubIntTy(rpos, rzero, rnan)) =>
      NumberIntTy(lnan || rnan)
    case (NumberIntTy(lnan), NumberSetTy(rset)) =>
      if (rset.forall(NumberIntTy(true).contains(_)))
        NumberIntTy(lnan || rset.exists(_.double.isNaN))
      else NumberTopTy
    case (
          NumberSubIntTy(lpos, lzero, lnan),
          NumberSubIntTy(rpos, rzero, rnan),
        ) =>
      if (lpos == rpos) NumberSubIntTy(lpos, lzero || rzero, lnan || rnan)
      else NumberIntTy(lnan || rnan)
    case (NumberSubIntTy(lpos, lzero, lnan), NumberSetTy(rset)) =>
      val posCheck: Double => Boolean = if (lpos) _ < 0 else _ > 0
      if (rset.forall(NumberSubIntTy(lpos, true, true).contains(_)))
        NumberSubIntTy(
          lpos,
          lzero || rset.exists(_.double == 0),
          lnan || rset.exists(_.double.isNaN),
        )
      else if (rset.forall(NumberIntTy(true).contains(_)))
        NumberIntTy(lnan || rset.exists(_.double.isNaN))
      else NumberTopTy
    case (NumberSetTy(lset), NumberSetTy(rset)) => NumberSetTy(lset union rset)
    case _                                      => that || this

  /** intersection type */
  def &&(that: => NumberTy): NumberTy = (this, that) match
    case _ if this eq that                      => this
    case (_, NumberTopTy)                       => this
    case (NumberTopTy, _)                       => that
    case (NumberIntTy(lnan), NumberIntTy(rnan)) => NumberIntTy(lnan && rnan)
    case (NumberIntTy(lnan), NumberSubIntTy(rpos, rzero, rnan)) =>
      NumberSubIntTy(rpos, rzero, lnan && rnan)
    case (NumberIntTy(lnan), NumberSetTy(rset)) =>
      NumberSetTy(rset.filter(this.contains(_)))
    case (
          NumberSubIntTy(lpos, lzero, lnan),
          NumberSubIntTy(rpos, rzero, rnan),
        ) =>
      if (lpos == rpos) NumberSubIntTy(lpos, lzero && rzero, lnan && rnan)
      else {
        var set: Set[Number] = Set.empty
        if (lzero && rzero) set += Number(0)
        if (lnan && rnan) set += Number(Double.NaN)
        NumberSetTy(set)
      }
    case (NumberSubIntTy(lpos, lzero, lnan), NumberSetTy(rset)) =>
      NumberSetTy(rset.filter(this.contains(_)))
    case (NumberSetTy(lset), NumberSetTy(rset)) =>
      NumberSetTy(lset intersect rset)
    case _ => that && this

  /** prune type */
  def --(that: => NumberTy): NumberTy = (this, that) match
    case _ if this eq that => Bot
    case (_, NumberTopTy)  => Bot
    case (NumberTopTy, _)  => Top
    case (NumberIntTy(lnan), NumberIntTy(rnan)) =>
      if (lnan && !rnan) NumberTy.NaN else Bot
    case (NumberIntTy(lnan), NumberSubIntTy(rpos, rzero, rnan)) =>
      NumberSubIntTy(!rpos, !rzero, lnan && !rnan)
    case (NumberIntTy(lnan), NumberSetTy(rset)) =>
      NumberIntTy(lnan && !rset.exists(_.double.isNaN))
    case (NumberSubIntTy(_, _, lnan), NumberIntTy(rnan)) =>
      if (lnan && rnan) NumberTy.NaN else Bot
    case (
          NumberSubIntTy(lpos, lzero, lnan),
          NumberSubIntTy(rpos, rzero, rnan),
        ) =>
      if (lpos == rpos)
        var set: Set[Number] = Set.empty
        if (lzero && !rzero) set += Number(0)
        if (lnan && !rnan) set += Number(Double.NaN)
        NumberSetTy(set)
      else NumberSubIntTy(lpos, lzero && !rzero, lnan && !rnan)
    case (NumberSubIntTy(lpos, lzero, lnan), NumberSetTy(rset)) =>
      NumberSubIntTy(
        lpos,
        lzero && !rset.exists(_.double == 0),
        lnan && !rset.exists(_.double.isNaN),
      )
    case (NumberSetTy(lset), NumberIntTy(rnan)) =>
      NumberSetTy(lset.filter(!that.contains(_)))
    case (NumberSetTy(lset), NumberSubIntTy(rpos, rzero, rnan)) =>
      NumberSetTy(lset.filter(!that.contains(_)))
    case (NumberSetTy(lset), NumberSetTy(rset)) => NumberSetTy(lset -- rset)
    case _                                      => this

  /** inclusion check */
  def contains(number: Number): Boolean = this match
    case NumberTopTy => true
    case NumberIntTy(nan) =>
      number.double.isWhole || (nan && number.double.isNaN)
    case NumberSubIntTy(pos, zero, nan) =>
      val posCheck: Double => Boolean = if (pos) _ > 0 else _ < 0
      (number.double.isWhole && posCheck(number.double)) ||
      (zero && number.double == 0) ||
      (nan && number.double.isNaN)
    case NumberSetTy(set) => set contains number

  /** get single value */
  def getSingle: Flat[Number] = this match
    case NumberSetTy(set) => Flat(set)
    case _                => Many

  /** addition */
  def +(that: NumberTy): NumberTy = (this, that) match
    case (l, r) if l.isPosInt && r.isPosInt       => PosInt
    case (l, r) if l.isNonNegInt && r.isNonNegInt => NonNegInt
    case (l, r) if l.isInt && r.isInt             => Int
    case _                                        => Top

  /** integral check */
  def isInt: Boolean = this match
    case NumberIntTy(false)          => true
    case NumberSubIntTy(_, _, false) => true
    case NumberSetTy(set)            => set.forall(n => n.double.isWhole)
    case _                           => false

  /** non-negative integral check */
  def isNonNegInt: Boolean = this match
    case NumberSubIntTy(true, _, false) => true
    case NumberSetTy(set) => set.forall(n => n.double.isWhole && n.double >= 0)
    case _                => false

  /** positive integral check */
  def isPosInt: Boolean = this match
    case NumberSubIntTy(true, false, false) => true
    case NumberSetTy(set) => set.forall(n => n.double.isWhole && n.double > 0)
    case _                => false

  /** non-positive integral check */
  def isNonPosInt: Boolean = this match
    case NumberSubIntTy(false, _, false) => true
    case NumberSetTy(set) => set.forall(n => n.double.isWhole && n.double <= 0)
    case _                => false

  /** negative integral check */
  def isNegInt: Boolean = this match
    case NumberSubIntTy(false, false, false) => true
    case NumberSetTy(set) => set.forall(n => n.double.isWhole && n.double < 0)
    case _                => false
}

/** number top types */
case object NumberTopTy extends NumberTy

/** integral number types */
case class NumberIntTy(hasNaN: Boolean) extends NumberTy

/** subset of integral number type */
case class NumberSubIntTy(
  pos: Boolean,
  hasZero: Boolean,
  hasNaN: Boolean,
) extends NumberTy

/** types for set of numbers */
case class NumberSetTy(set: Set[Number]) extends NumberTy

object NumberTy extends Parser.From(Parser.numberTy) {
  lazy val Top: NumberTy = NumberTopTy
  lazy val Bot: NumberTy = NumberSetTy(Set.empty)
  lazy val Int: NumberTy = NumberIntTy(false)
  lazy val NonPosInt: NumberTy = NumberSubIntTy(false, true, false)
  lazy val NonNegInt: NumberTy = NumberSubIntTy(true, true, false)
  lazy val NegInt: NumberTy = NumberSubIntTy(false, false, false)
  lazy val PosInt: NumberTy = NumberSubIntTy(true, false, false)
  lazy val Zero: NumberTy = NumberSetTy(Set(Number(0)))
  lazy val One: NumberTy = NumberSetTy(Set(Number(1)))
  lazy val NaN: NumberTy = NumberSetTy(Set(Number(Double.NaN)))
}
