package esmeta.ty

import esmeta.util.*
import esmeta.ty.util.Parser

/** record types */
enum RecordTy extends TyElem with Lattice[RecordTy] {

  /** a detailed record type with its type name and extended fields */
  case Detail(name: String, map: Map[String, ValueTy])

  /** a simple record type with a set of record names */
  case Simple(set: Set[String])

  import ManualInfo.tyModel.*
  import RecordTy.*

  /** top check */
  def isTop: Boolean = this == Top

  /** bottom check */
  def isBottom: Boolean = this == Bot

  /** partial order/subset operator */
  def <=(that: => RecordTy): Boolean = (this, that) match
    case _ if this eq that => true
    case (Detail(l, lmap), Detail(r, rmap)) =>
      (l == r && rmap.forall { this(_) <= _ }) || isSubTy(l, r)
    case (Detail(l, _), Simple(rs)) => isSubTy(l, rs)
    case (Simple(ls), Simple(rs))   => isSubTy(ls, rs)
    case (Simple(ls), Detail(r, map)) =>
      isSubTy(ls, r) && map.forall { this(_) <= _ }

  /** union type */
  def ||(that: => RecordTy): RecordTy =
    if (this <= that) that
    else if (that <= this) this
    else
      val (ls, rs) = (this.names, that.names)
      Simple(ls.filter(!isSubTy(_, rs)) ++ rs.filter(!isSubTy(_, ls)))

  /** intersection type */
  def &&(that: => RecordTy): RecordTy =
    if (this <= that) this
    else if (that <= this) that
    else
      val (ls, rs) = (this.names, that.names)
      Simple(ls.filter(isSubTy(_, rs)) ++ rs.filter(isSubTy(_, ls)))

  /** prune type */
  def --(that: => RecordTy): RecordTy = this match
    case Simple(set) => Simple(set.filter(!isSubTy(_, that.names)))
    case _           => this

  /** fields */
  def fieldMap: Map[String, Ty] = this match
    case Detail(name, map) => getFieldMap(name).map ++ map
    case Simple(set) =>
      val fields = for {
        name <- set
        (field, _) <- getFieldMap(name).map
      } yield field
      fields.toList.map(f => f -> apply(f)).toMap

  /** base type names */
  def bases: Set[String] = this match
    case Detail(name, _) => Set(getBase(name))
    case Simple(set)     => set.map(getBase)

  /** type names */
  def names: Set[String] = this match
    case Detail(name, _) => Set(name)
    case Simple(set)     => set

  /** field accessor */
  def apply(field: String): ValueTy = this match
    case Detail(name, map) => map.getOrElse(field, getField(name, field))
    case Simple(set)       => set.foldLeft(BotT)(_ || getField(_, field))
}

object RecordTy extends Parser.From(Parser.recordTy) {
  lazy val Top: RecordTy = Simple(Set(""))
  lazy val Bot: RecordTy = Simple(Set.empty)
  def apply(names: String*): RecordTy =
    apply(names.toSet)
  def apply(names: Set[String]): RecordTy =
    if (names.isEmpty) Bot else Simple(names)
  def apply(name: String, fields: Map[String, ValueTy]): RecordTy =
    if (fields.isEmpty) Simple(Set(name)) else Detail(name, fields)
  def apply(fields: Map[String, ValueTy]): RecordTy =
    if (fields.isEmpty) Top else Detail("", fields)
}
