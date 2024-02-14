package esmeta.ty

import esmeta.state.*
import esmeta.ty.util.Parser
import esmeta.util.*

/** mathematical value types */
sealed trait MathTy extends TyElem with Lattice[MathTy] {
  import MathTy.*

  /** top check */
  def isTop: Boolean = this eq Top

  /** bottom check */
  def isBottom: Boolean = this == Bot

  /** partial order/subset operator */
  def <=(that: => MathTy): Boolean = (this, that) match
    case _ if (this eq that) || (this == Bot) => true
    case (_, MathTopTy)                       => true
    case (NegIntTy | PosIntTy | NonNegIntTy | NonPosIntTy | IntTy, IntTy) =>
      true
    case (MathSetTy(set), IntTy)               => set.forall(d => d.isWhole)
    case (NegIntTy | NonPosIntTy, NonPosIntTy) => true
    case (MathSetTy(set), NonPosIntTy) => set.forall(d => d.isWhole && d <= 0)
    case (PosIntTy | NonNegIntTy, NonNegIntTy) => true
    case (MathSetTy(set), NonNegIntTy) => set.forall(d => d.isWhole && d >= 0)
    case (NegIntTy, NegIntTy)          => true
    case (MathSetTy(set), NegIntTy)    => set.forall(d => d.isWhole && d < 0)
    case (PosIntTy, PosIntTy)          => true
    case (MathSetTy(set), PosIntTy)    => set.forall(d => d.isWhole && d > 0)
    case (MathSetTy(lset), MathSetTy(rset)) => lset subsetOf rset
    case _                                  => false

  /** union type */
  def ||(that: => MathTy): MathTy = (this, that) match
    case _ if this eq that               => this
    case (MathTopTy, _) | (_, MathTopTy) => Top
    case (IntTy, MathSetTy(set)) =>
      if (set.exists(d => d.isWhole)) Top
      else IntTy
    case (IntTy, _) => IntTy
    case (NonPosIntTy, MathSetTy(set)) =>
      if (set.exists(d => !d.isWhole)) Top
      else if (set.exists(d => d >= 0)) IntTy
      else NonPosIntTy
    case (NonPosIntTy, PosIntTy | NonNegIntTy | IntTy) => IntTy
    case (NonPosIntTy, _)                              => NonPosIntTy
    case (NonNegIntTy, MathSetTy(set)) =>
      if (set.exists(d => !d.isWhole)) Top
      else if (set.exists(d => d < 0)) IntTy
      else NonNegIntTy
    case (NonNegIntTy, NegIntTy | NonPosIntTy | IntTy) => IntTy
    case (NonNegIntTy, _)                              => NonNegIntTy
    case (NegIntTy, MathSetTy(set)) =>
      if (set.exists(d => !d.isWhole)) Top
      else if (set.exists(d => d > 0)) IntTy
      else if (set contains 0) NonNegIntTy
      else NegIntTy
    case (NegIntTy, PosIntTy | NonNegIntTy | IntTy) => IntTy
    case (NegIntTy, NonPosIntTy)                    => NonPosIntTy
    case (NegIntTy, _)                              => NegIntTy
    case (PosIntTy, MathSetTy(set)) =>
      if (set.exists(d => !d.isWhole)) Top
      else if (set.exists(d => d < 0)) IntTy
      else if (set contains 0) NonNegIntTy
      else PosIntTy
    case (PosIntTy, NegIntTy | NonPosIntTy | IntTy) => IntTy
    case (PosIntTy, NonNegIntTy)                    => NonNegIntTy
    case (PosIntTy, _)                              => PosIntTy
    case (MathSetTy(lset), MathSetTy(rset)) => MathSetTy(lset union rset)
    case (MathSetTy(set), _)                => that || this

  /** intersection type */
  def &&(that: => MathTy): MathTy = (this, that) match
    case _ if this eq that          => this
    case (_, MathTopTy)             => this
    case (MathTopTy, _)             => that
    case (IntTy, MathSetTy(set))    => MathSetTy(set.filter(d => d.isWhole))
    case (IntTy, _)                 => that
    case (NonPosIntTy, IntTy)       => NonPosIntTy
    case (NonPosIntTy, NonNegIntTy) => Zero
    case (NonPosIntTy, PosIntTy)    => Bot
    case (NonPosIntTy, MathSetTy(set)) =>
      MathSetTy(set.filter(d => d.isWhole && d <= 0))
    case (NonPosIntTy, _)        => that
    case (NonNegIntTy, IntTy)    => NonNegIntTy
    case (NonNegIntTy, NegIntTy) => Zero
    case (NonNegIntTy, PosIntTy) => Bot
    case (NonNegIntTy, MathSetTy(set)) =>
      MathSetTy(set.filter(d => d.isWhole && d >= 0))
    case (NonNegIntTy, _)                   => that
    case (NegIntTy, NonPosIntTy | IntTy)    => NegIntTy
    case (NegIntTy, NonNegIntTy | PosIntTy) => Bot
    case (NegIntTy, MathSetTy(set)) =>
      MathSetTy(set.filter(d => d.isWhole && d < 0))
    case (NegIntTy, _)                      => that
    case (PosIntTy, NonNegIntTy | IntTy)    => PosIntTy
    case (PosIntTy, NonPosIntTy | NegIntTy) => Bot
    case (PosIntTy, MathSetTy(set)) =>
      MathSetTy(set.filter(d => d.isWhole && d > 0))
    case (PosIntTy, _)                      => that
    case (MathSetTy(lset), MathSetTy(rset)) => MathSetTy(lset intersect rset)
    case _                                  => that && this

  /** prune type */
  def --(that: => MathTy): MathTy = (this, that) match
    case _ if this eq that                          => Bot
    case (_, MathTopTy)                             => Bot
    case (MathTopTy, _)                             => Top
    case (IntTy, IntTy)                             => Bot
    case (IntTy, NonPosIntTy)                       => PosIntTy
    case (IntTy, NonNegIntTy)                       => NegIntTy
    case (IntTy, NegIntTy)                          => NonNegIntTy
    case (IntTy, PosIntTy)                          => NonPosIntTy
    case (IntTy, _)                                 => IntTy
    case (NonPosIntTy, NonPosIntTy | IntTy)         => Bot
    case (NonPosIntTy, NonNegIntTy)                 => NegIntTy
    case (NonPosIntTy, NegIntTy)                    => Zero
    case (NonPosIntTy, _)                           => NonPosIntTy
    case (NonNegIntTy, NonNegIntTy | IntTy)         => Bot
    case (NonNegIntTy, NonPosIntTy)                 => PosIntTy
    case (NonNegIntTy, PosIntTy)                    => Zero
    case (NonNegIntTy, _)                           => NonNegIntTy
    case (NegIntTy, NegIntTy | NonPosIntTy | IntTy) => Bot
    case (NegIntTy, _)                              => NegIntTy
    case (PosIntTy, PosIntTy | NonNegIntTy | IntTy) => Bot
    case (PosIntTy, _)                              => PosIntTy
    case (MathSetTy(set), IntTy) =>
      MathSetTy(set.filter(d => !d.isWhole))
    case (MathSetTy(set), NonPosIntTy) =>
      MathSetTy(set.filter(d => !(d.isWhole && d <= 0)))
    case (MathSetTy(set), NonNegIntTy) =>
      MathSetTy(set.filter(d => !(d.isWhole && d >= 0)))
    case (MathSetTy(set), NegIntTy) =>
      MathSetTy(set.filter(d => !(d.isWhole && d < 0)))
    case (MathSetTy(set), PosIntTy) =>
      MathSetTy(set.filter(d => !(d.isWhole && d > 0)))
    case (MathSetTy(lset), MathSetTy(rset)) => MathSetTy(lset -- rset)

  /** inclusion check */
  def contains(decimal: BigDecimal): Boolean = this match
    case MathTopTy      => true
    case IntTy          => decimal.isWhole
    case NonPosIntTy    => decimal.isWhole && decimal <= 0
    case NonNegIntTy    => decimal.isWhole && decimal >= 0
    case NegIntTy       => decimal.isWhole && decimal < 0
    case PosIntTy       => decimal.isWhole && decimal > 0
    case MathSetTy(set) => set contains decimal

  /** get single value */
  def getSingle: Flat[BigDecimal] = this match
    case MathSetTy(set) => Flat(set)
    case _              => Many
}

/** mathematical value types */
case object MathTopTy extends MathTy

/** integer types */
case object IntTy extends MathTy

/** non-positive integer type */
case object NonPosIntTy extends MathTy

/** non-negative integer type */
case object NonNegIntTy extends MathTy

/** negative integer type */
case object NegIntTy extends MathTy

/** positive integer type */
case object PosIntTy extends MathTy

/** types for set of mathematical values */
case class MathSetTy(set: Set[BigDecimal]) extends MathTy

object MathTy extends Parser.From(Parser.mathTy) {
  lazy val Top: MathTy = MathTopTy
  lazy val Bot: MathTy = MathSetTy(Set.empty)
  lazy val Zero: MathTy = MathSetTy(Set(0))
}
