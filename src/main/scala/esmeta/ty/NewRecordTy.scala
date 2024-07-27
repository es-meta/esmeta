package esmeta.ty

import esmeta.util.*
import esmeta.ty.util.Parser

/** record types */
enum NewRecordTy extends TyElem with Lattice[NewRecordTy] {

  /** a detailed record type with its type name and extended fields */
  case Detail(name: String, map: Map[String, ValueTy])

  /** a simple record type with a set of record names */
  case Simple(set: Set[String])

  import ManualInfo.tyModel.*
  import NewRecordTy.*

  /** top check */
  def isTop: Boolean = this == Top

  /** bottom check */
  def isBottom: Boolean = this == Bot

  /** partial order/subset operator */
  def <=(that: => NewRecordTy): Boolean = (this, that) match
    case _ if this eq that => true
    case (Detail(l, lmap), Detail(r, rmap)) =>
      (l == r && rmap.forall { this(_) <= _ }) || isSubTy(l, r)
    case (Detail(l, _), Simple(rs)) => isSubTy(l, rs)
    case (Simple(ls), Simple(rs))   => isSubTy(ls, rs)
    case (Simple(ls), Detail(r, map)) =>
      isSubTy(ls, r) && map.forall { this(_) <= _ }

  /** union type */
  def ||(that: => NewRecordTy): NewRecordTy =
    val (ls, rs) = (this.names, that.names)
    Simple(ls.filter(!isSubTy(_, rs)) ++ rs.filter(!isSubTy(_, ls)))

  /** intersection type */
  def &&(that: => NewRecordTy): NewRecordTy =
    val (ls, rs) = (this.names, that.names)
    Simple(ls.filter(isSubTy(_, rs)) ++ rs.filter(isSubTy(_, ls)))

  /** prune type */
  def --(that: => NewRecordTy): NewRecordTy =
    Simple(this.names.filter(!isSubTy(_, that.names)))

  /** type names */
  def names: Set[String] = this match
    case Detail(name, _) => Set(name)
    case Simple(set)     => set

  /** field accessor */
  def apply(field: String): ValueTy = this match
    case Detail(name, map) => map.getOrElse(field, getField(name, field))
    case Simple(set)       => set.foldLeft(BotT)(_ || getField(_, field))
}

object NewRecordTy extends Parser.From(???) {
  lazy val Top: NewRecordTy = Simple(Set(""))
  lazy val Bot: NewRecordTy = Simple(Set.empty)
}
