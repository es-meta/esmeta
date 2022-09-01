package esmeta.ty

import esmeta.util.*
import esmeta.ty.util.Parser

/** record types */
case class RecordTy(
  fields: Set[String] = Set(),
  map: Map[String, ValueTy] = Map(),
) extends TyElem
  with Lattice[RecordTy] {

  /** bottom check */
  def isBottom: Boolean =
    this.fields.isBottom &
    this.map.isBottom

  /** partial order/subset operator */
  def <=(that: => RecordTy): Boolean =
    this.fields <= that.fields &
    this.map <= that.map

  /** union type */
  def |(that: => RecordTy): RecordTy = RecordTy(
    this.fields | that.fields,
    this.map | that.map,
  )

  /** intersection type */
  def &(that: => RecordTy): RecordTy = RecordTy(
    this.fields & that.fields,
    this.map & that.map,
  ).norm

  /** prune type */
  def --(that: => RecordTy): RecordTy = RecordTy(
    this.fields -- that.fields,
    this.map -- that.map,
  ).norm

  // normalization
  def norm: RecordTy = RecordTy(
    this.fields,
    this.map.filter { case (_, v) => !v.isBottom },
  )
}
object RecordTy extends Parser.From(Parser.recordTy)
