package esmeta.ty

import esmeta.util.*
import esmeta.ty.util.Parser

/** record types */
case class RecordTy(
  names: Set[String] = Set(),
  fields: Set[String] = Set(),
  map: Map[String, ValueTy] = Map(),
) extends TyElem
  with Lattice[RecordTy] {

  /** bottom check */
  def isBottom: Boolean =
    this.names.isEmpty &
    this.fields.isEmpty &
    this.map.isEmpty

  /** partial order/subset operator */
  def <=(that: => RecordTy): Boolean =
    this.names <= that.names &
    this.fields <= that.fields &
    this.map <= that.map

  /** union type */
  def |(that: => RecordTy): RecordTy = RecordTy(
    this.names | that.names,
    this.fields | that.fields,
    this.map | that.map,
  )

  /** intersection type */
  def &(that: => RecordTy): RecordTy = RecordTy(
    this.names & that.names,
    this.fields & that.fields,
    this.map & that.map,
  ).norm

  /** prune type */
  def --(that: => RecordTy): RecordTy = RecordTy(
    this.names -- that.names,
    this.fields -- that.fields,
    this.map -- that.map,
  ).norm

  // normalization
  def norm: RecordTy = RecordTy(
    this.names,
    this.fields,
    this.map.filter { case (_, v) => !v.isBottom },
  )
}
object RecordTy extends Parser.From[RecordTy](Parser.recordTy)
