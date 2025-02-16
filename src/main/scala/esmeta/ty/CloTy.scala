package esmeta.ty

import esmeta.state.*
import esmeta.ty.util.Parser
import esmeta.util.*
import esmeta.domain.{*, given}, BSet.*, Flat.*

/** closure types */
sealed trait CloTy extends TyElem {
  import CloTy.*

  /** top check */
  def isTop: Boolean = this eq Top

  /** bottom check */
  def isBottom: Boolean = this == Bot

  /** partial order/subset operator */
  def <=(that: CloTy): Boolean = (this, that) match
    case (_, Top) => true
    case (CloArrowTy(lps, lret), CloArrowTy(rps, rret)) =>
      lps.zip(rps).forall((l, r) => l <= r) && rret <= lret
    case (CloSetTy(lset), CloSetTy(rset)) => lset subsetOf rset
    case (_: CloSetTy, _: CloArrowTy)     => true
    case _                                => false

  /** union type */
  def ||(that: CloTy): CloTy = (this, that) match
    case (CloTopTy, _) | (_, CloTopTy) => Top
    case (CloArrowTy(lps, lret), CloArrowTy(rps, rret)) =>
      CloArrowTy(lps.zip(rps).map((l, r) => l || r), lret && rret)
    case (CloSetTy(lset), CloSetTy(rset)) => CloSetTy(lset ++ rset)
    case (_: CloSetTy, _: CloArrowTy)     => that
    case (_: CloArrowTy, _: CloSetTy)     => this

  /** intersection type */
  def &&(that: CloTy): CloTy = (this, that) match
    case (CloTopTy, _) => that
    case (_, CloTopTy) => this
    case (CloArrowTy(lps, lret), CloArrowTy(rps, rret)) =>
      CloArrowTy(lps.zip(rps).map((l, r) => l && r), lret || rret)
    case (CloSetTy(lset), CloSetTy(rset)) => CloSetTy(lset intersect rset)
    case (_: CloSetTy, _: CloArrowTy)     => this
    case (_: CloArrowTy, _: CloSetTy)     => that

  /** prune type */
  def --(that: CloTy): CloTy = (this, that) match
    case _ if this <= that                => Bot
    case (CloSetTy(lset), CloSetTy(rset)) => CloSetTy(lset -- rset)
    case _                                => this

  /** inclusion check */
  def contains(fname: String): Boolean = this match
    case CloSetTy(names) => names contains fname
    case _               => true

  /** flatten */
  def toFlat: Flat[Clo] = if (isBottom) Zero else Many

  /** to list of atomic closure types */
  def toAtomicTys: List[CloTy] = if (isBottom) Nil else List(this)
}
object CloTy extends Parser.From(Parser.cloTy) {
  lazy val Top: CloTy = CloTopTy
  lazy val Bot: CloTy = CloSetTy(Set.empty)
}

/** closure types */
case object CloTopTy extends CloTy

/** closure types with types with parameter types and return type */
case class CloArrowTy(params: List[ValueTy], ret: ValueTy) extends CloTy

/** closure set types */
case class CloSetTy(names: Set[String]) extends CloTy
