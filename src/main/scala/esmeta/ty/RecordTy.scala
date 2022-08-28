package esmeta.ty

import esmeta.util.*

/** record types */
case class RecordTy(
  names: Set[String] = Set(),
  map: Map[String, ValueTy] = Map(),
) extends TyElem
  with Lattice[RecordTy] {

  /** bottom check */
  def isBottom: Boolean =
    this.names.isEmpty &
    this.map.isEmpty

  /** partial order/subset operator */
  def <=(that: => RecordTy): Boolean =
    this.names <= that.names &
    this.map <= that.map

  /** union type */
  def |(that: => RecordTy): RecordTy = RecordTy(
    this.names | that.names,
    this.map | that.map,
  )

  /** intersection type */
  def &(that: => RecordTy): RecordTy = RecordTy(
    this.names & that.names,
    this.map & that.map,
  )

  /** prune type */
  def --(that: => RecordTy): RecordTy = RecordTy(
    this.names -- that.names,
    this.map -- that.map,
  )
}
