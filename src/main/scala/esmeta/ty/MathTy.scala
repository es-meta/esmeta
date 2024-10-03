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
    case (MathSetTy(set), IntTy) =>
      set.forall(_.decimal.isWhole)
    case (NegIntTy | NonPosIntTy, NonPosIntTy) => true
    case (MathSetTy(set), NonPosIntTy) =>
      set.forall(m => m.decimal.isWhole && m.decimal <= 0)
    case (PosIntTy | NonNegIntTy, NonNegIntTy) => true
    case (MathSetTy(set), NonNegIntTy) =>
      set.forall(m => m.decimal.isWhole && m.decimal >= 0)
    case (NegIntTy, NegIntTy) => true
    case (MathSetTy(set), NegIntTy) =>
      set.forall(m => m.decimal.isWhole && m.decimal < 0)
    case (PosIntTy, PosIntTy) => true
    case (MathSetTy(set), PosIntTy) =>
      set.forall(m => m.decimal.isWhole && m.decimal > 0)
    case (MathSetTy(lset), MathSetTy(rset)) => lset subsetOf rset
    case _                                  => false

  /** union type */
  def ||(that: => MathTy): MathTy = (this, that) match
    case _ if this eq that               => this
    case (MathTopTy, _) | (_, MathTopTy) => Top
    case (IntTy, MathSetTy(set)) =>
      if (set.exists(m => !m.decimal.isWhole)) Top
      else IntTy
    case (IntTy, _) => IntTy
    case (NonPosIntTy, MathSetTy(set)) =>
      if (set.exists(m => !m.decimal.isWhole)) Top
      else if (set.exists(m => m.decimal >= 0)) IntTy
      else NonPosIntTy
    case (NonPosIntTy, PosIntTy | NonNegIntTy | IntTy) => IntTy
    case (NonPosIntTy, _)                              => NonPosIntTy
    case (NonNegIntTy, MathSetTy(set)) =>
      if (set.exists(m => !m.decimal.isWhole)) Top
      else if (set.exists(m => m.decimal < 0)) IntTy
      else NonNegIntTy
    case (NonNegIntTy, NegIntTy | NonPosIntTy | IntTy) => IntTy
    case (NonNegIntTy, _)                              => NonNegIntTy
    case (NegIntTy, MathSetTy(set)) =>
      if (set.exists(m => !m.decimal.isWhole)) Top
      else if (set.exists(m => m.decimal > 0)) IntTy
      else if (set contains Math.zero) NonNegIntTy
      else NegIntTy
    case (NegIntTy, PosIntTy | NonNegIntTy | IntTy) => IntTy
    case (NegIntTy, NonPosIntTy)                    => NonPosIntTy
    case (NegIntTy, _)                              => NegIntTy
    case (PosIntTy, MathSetTy(set)) =>
      if (set.exists(m => !m.decimal.isWhole)) Top
      else if (set.exists(m => m.decimal < 0)) IntTy
      else if (set contains Math.zero) NonNegIntTy
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
    case (IntTy, MathSetTy(set))    => MathSetTy(set.filter(_.decimal.isWhole))
    case (IntTy, _)                 => that
    case (NonPosIntTy, IntTy)       => NonPosIntTy
    case (NonPosIntTy, NonNegIntTy) => Zero
    case (NonPosIntTy, PosIntTy)    => Bot
    case (NonPosIntTy, MathSetTy(set)) =>
      MathSetTy(set.filter(m => m.decimal.isWhole && m.decimal <= 0))
    case (NonPosIntTy, _)        => that
    case (NonNegIntTy, IntTy)    => NonNegIntTy
    case (NonNegIntTy, NegIntTy) => Zero
    case (NonNegIntTy, PosIntTy) => PosIntTy
    case (NonNegIntTy, MathSetTy(set)) =>
      MathSetTy(set.filter(m => m.decimal.isWhole && m.decimal >= 0))
    case (NonNegIntTy, NonPosIntTy)         => Zero
    case (NegIntTy, NonPosIntTy | IntTy)    => NegIntTy
    case (NegIntTy, NonNegIntTy | PosIntTy) => Bot
    case (NegIntTy, MathSetTy(set)) =>
      MathSetTy(set.filter(m => m.decimal.isWhole && m.decimal < 0))
    case (NegIntTy, _)                      => that
    case (PosIntTy, NonNegIntTy | IntTy)    => PosIntTy
    case (PosIntTy, NonPosIntTy | NegIntTy) => Bot
    case (PosIntTy, MathSetTy(set)) =>
      MathSetTy(set.filter(m => m.decimal.isWhole && m.decimal > 0))
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
      MathSetTy(set.filter(m => !m.decimal.isWhole))
    case (MathSetTy(set), NonPosIntTy) =>
      MathSetTy(set.filter(m => !(m.decimal.isWhole && m.decimal <= 0)))
    case (MathSetTy(set), NonNegIntTy) =>
      MathSetTy(set.filter(m => !(m.decimal.isWhole && m.decimal >= 0)))
    case (MathSetTy(set), NegIntTy) =>
      MathSetTy(set.filter(m => !(m.decimal.isWhole && m.decimal < 0)))
    case (MathSetTy(set), PosIntTy) =>
      MathSetTy(set.filter(m => !(m.decimal.isWhole && m.decimal > 0)))
    case (MathSetTy(lset), MathSetTy(rset)) => MathSetTy(lset -- rset)

  /** addition */
  def +(that: MathTy): MathTy = (this, that) match
    case (l, r) if l.isNonNegInt && r.isNonNegInt => NonNegIntTy
    case (l, r) if l.isInt && r.isInt             => IntTy
    case _                                        => MathTopTy

  /** subtraction */
  def -(that: MathTy): MathTy = (this, that) match
    case (SingleTy(Math(l)), SingleTy(Math(r))) => MathSetTy(Math(l - r))
    case (l, r) if l.isInt && r.isInt           => IntTy
    case _                                      => MathTopTy

  /** multiplcation */
  def *(that: MathTy): MathTy = (this, that) match
    case (l, r) if l.isNonNegInt && r.isNonNegInt => NonNegIntTy
    case (l, r) if l.isInt && r.isInt             => IntTy
    case _                                        => MathTopTy

  /** modulo */
  def %(that: MathTy): MathTy = (this, that) match
    case (l, r) if l.isNonNegInt && r.isNonNegInt => NonNegIntTy
    case (l, r) if l.isInt && r.isInt             => IntTy
    case _                                        => MathTopTy

  /** exponentiation */
  def **(that: MathTy): MathTy = (this, that) match
    case (SingleTy(Math(l)), SingleTy(Math(r))) if r.isValidInt && r >= 0 =>
      MathSetTy(Math(l.pow(r.toInt)))
    case (l, r) if l.isNonNegInt && r.isNonNegInt => NonNegIntTy
    case (l, r) if l.isInt && r.isInt             => IntTy
    case _                                        => MathTopTy

  /** bitwise operation (&) */
  def &(that: MathTy): MathTy = (this, that) match
    case (l, r) if l.isInt && r.isInt => IntTy
    case _                            => MathTopTy

  /** bitwise operation (|) */
  def |(that: MathTy): MathTy = (this, that) match
    case (l, r) if l.isInt && r.isInt => IntTy
    case _                            => MathTopTy

  /** bitwise operation (^) */
  def ^(that: MathTy): MathTy = (this, that) match
    case (l, r) if l.isInt && r.isInt => IntTy
    case _                            => MathTopTy

  /** shift left */
  def <<(that: MathTy): MathTy = (this, that) match
    case (l, r) if l.isInt && r.isInt => IntTy
    case _                            => MathTopTy

  /** shift right */
  def >>(that: MathTy): MathTy = (this, that) match
    case (l, r) if l.isInt && r.isInt => IntTy
    case _                            => MathTopTy

  /** unsigned shift right */
  def >>>(that: MathTy): MathTy = (this, that) match
    case (l, r) if l.isInt && r.isInt => IntTy
    case _                            => MathTopTy

  /** min operation */
  def min(that: MathTy): MathTy = (this, that) match
    case (MathTopTy, _) | (_, MathTopTy)         => MathTopTy
    case (IntTy, IntTy | NonNegIntTy | PosIntTy) => IntTy
    case (IntTy, NonPosIntTy)                    => NonPosIntTy
    case (IntTy, NegIntTy)                       => NegIntTy
    case (NonNegIntTy, NonNegIntTy | PosIntTy)   => NonNegIntTy
    case (NonNegIntTy, NonPosIntTy)              => NonPosIntTy
    case (NonNegIntTy, NegIntTy)                 => NegIntTy
    case (NonPosIntTy, NonPosIntTy | PosIntTy)   => NonPosIntTy
    case (NonPosIntTy, NegIntTy)                 => NegIntTy
    case (PosIntTy, PosIntTy)                    => PosIntTy
    case (PosIntTy, NegIntTy)                    => NegIntTy
    case (NegIntTy, NegIntTy)                    => NegIntTy
    case (MathSetTy(set), _)                     => set.foldLeft(that)(_ min _)
    case _                                       => that min this

  /** min operation */
  private def min(math: Math): MathTy =
    val Math(d) = math
    this match
      case MathTopTy       => MathTopTy
      case MathSetTy(set)  => MathSetTy(set.map(m => Math(m.decimal min d)))
      case _ if !d.isWhole => MathTopTy
      case IntTy if d < 0  => NegIntTy
      case IntTy if d <= 0 => NonPosIntTy
      case IntTy           => IntTy
      case NonNegIntTy if d <= 0 => MathSetTy(Set(math))
      case NonNegIntTy           => NonNegIntTy
      case NonPosIntTy if d < 0  => NegIntTy
      case NonPosIntTy           => NonPosIntTy
      case PosIntTy if d <= 1    => MathSetTy(Set(math))
      case PosIntTy              => PosIntTy
      case NegIntTy              => NegIntTy

  /** max operation */
  def max(that: MathTy): MathTy = (this, that) match
    case (MathTopTy, _) | (_, MathTopTy)       => MathTopTy
    case (IntTy, IntTy)                        => IntTy
    case (IntTy, NonPosIntTy | NegIntTy)       => IntTy
    case (IntTy, NonNegIntTy)                  => NonNegIntTy
    case (IntTy, PosIntTy)                     => PosIntTy
    case (PosIntTy, PosIntTy | NonNegIntTy)    => PosIntTy
    case (PosIntTy, NonPosIntTy | NegIntTy)    => PosIntTy
    case (NonNegIntTy, NonNegIntTy)            => NonNegIntTy
    case (NonNegIntTy, NonPosIntTy | NegIntTy) => NonNegIntTy
    case (NonPosIntTy, NonPosIntTy | NegIntTy) => NonPosIntTy
    case (NegIntTy, NegIntTy)                  => NegIntTy
    case (MathSetTy(set), _)                   => set.foldLeft(that)(_ max _)
    case _                                     => that max this

  /** max operation */
  private def max(math: Math): MathTy =
    val Math(d) = math
    this match
      case MathTopTy       => MathTopTy
      case MathSetTy(set)  => MathSetTy(set.map(m => Math(m.decimal max d)))
      case _ if !d.isWhole => MathTopTy
      case IntTy if d > 0  => PosIntTy
      case IntTy if d >= 0 => NonNegIntTy
      case IntTy           => IntTy
      case NonPosIntTy if d >= 0 => MathSetTy(Set(math))
      case NonPosIntTy           => NonPosIntTy
      case NonNegIntTy if d > 0  => PosIntTy
      case NonNegIntTy           => NonNegIntTy
      case NegIntTy if d >= -1   => MathSetTy(Set(math))
      case NegIntTy              => NegIntTy
      case PosIntTy              => PosIntTy

  /** inclusion check */
  def contains(math: Math): Boolean = this match
    case MathTopTy      => true
    case IntTy          => math.decimal.isWhole
    case NonPosIntTy    => math.decimal.isWhole && math.decimal <= 0
    case NonNegIntTy    => math.decimal.isWhole && math.decimal >= 0
    case NegIntTy       => math.decimal.isWhole && math.decimal < 0
    case PosIntTy       => math.decimal.isWhole && math.decimal > 0
    case MathSetTy(set) => set contains math

  /** get single value */
  def getSingle: Flat[Math] = this match
    case MathSetTy(set) => Flat(set)
    case _              => Many

  /** integral check */
  def isInt: Boolean = this match
    case IntTy | NonPosIntTy | NonNegIntTy | NegIntTy | PosIntTy => true
    case MathSetTy(set) => set.forall(n => n.decimal.isWhole)
    case _              => false

  /** non-positive integral check */
  def isNonPosInt: Boolean = this match
    case NonPosIntTy    => true
    case MathSetTy(set) => set.forall(n => n.decimal.isWhole && n.decimal <= 0)
    case _              => false

  /** non-negative integral check */
  def isNonNegInt: Boolean = this match
    case NonNegIntTy    => true
    case MathSetTy(set) => set.forall(n => n.decimal.isWhole && n.decimal >= 0)
    case _              => false

  /** negative integral check */
  def isNegInt: Boolean = this match
    case NegIntTy       => true
    case MathSetTy(set) => set.forall(n => n.decimal.isWhole && n.decimal < 0)
    case _              => false

  /** positive integral check */
  def isPosInt: Boolean = this match
    case PosIntTy       => true
    case MathSetTy(set) => set.forall(n => n.decimal.isWhole && n.decimal > 0)
    case _              => false
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
case class MathSetTy(set: Set[Math]) extends MathTy
object SingleTy {
  def unapply(ty: MathSetTy): Option[Math] =
    if (ty.set.size == 1) Some(ty.set.head) else None
}
object MathSetTy {
  def apply(seq: Math*): MathSetTy = MathSetTy(seq.toSet)
}

object MathTy extends Parser.From(Parser.mathTy) {
  lazy val Top: MathTy = MathTopTy
  lazy val Bot: MathTy = MathSetTy(Set.empty)
  lazy val Zero: MathTy = MathSetTy(Set(Math.zero))
  lazy val One: MathTy = MathSetTy(Set(Math.one))
}
