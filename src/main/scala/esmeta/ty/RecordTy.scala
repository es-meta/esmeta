package esmeta.ty

import esmeta.util.*
import esmeta.ty.util.Parser

/** record types */
sealed trait RecordTy extends TyElem with Lattice[RecordTy] {
  import RecordTy.*

  /** top check */
  def isTop: Boolean = this eq Top

  /** bottom check */
  def isBottom: Boolean = this == Bot

  /** partial order/subset operator */
  def <=(that: => RecordTy): Boolean = (this, that) match
    case _ if this eq that => true
    case (_, Top)          => true
    case (Top, _)          => false
    case (Elem(lmap), Elem(rmap)) =>
      (lmap.keySet ++ rmap.keySet).forall(field => this(field) <= that(field))

  /** union type */
  def ||(that: => RecordTy): RecordTy = (this, that) match
    case _ if this eq that   => this
    case (Top, _) | (_, Top) => Top
    case (Elem(lmap), Elem(rmap)) =>
      Elem(
        (for {
          field <- (lmap.keySet ++ rmap.keySet).toList
        } yield field -> (this(field) || that(field))).toMap,
      )

  /** intersection type */
  def &&(that: => RecordTy): RecordTy = (this, that) match
    case _ if this eq that   => this
    case (Top, _) | (_, Top) => Top
    case (Elem(lmap), Elem(rmap)) =>
      Elem(
        (for {
          field <- (lmap.keySet ++ rmap.keySet).toList
        } yield field -> (this(field) && that(field))).toMap,
      ).normalized

  /** prune type */
  def --(that: => RecordTy): RecordTy = (this, that) match
    case _ if this eq that   => this
    case (Top, _) | (_, Top) => Top
    case (Elem(lmap), Elem(rmap)) =>
      Elem(
        (for {
          field <- (lmap.keySet ++ rmap.keySet).toList
        } yield field -> (this(field) -- that(field))).toMap,
      ).normalized

  /** field accessor */
  def apply(field: String): ValueTy = this match
    case Top       => ValueTy.Top
    case Elem(map) => map.getOrElse(field, ValueTy.Bot)

  /** normalized type */
  def normalized: RecordTy = this match
    case Top => Top
    case Bot => Bot
    case Elem(map) =>
      val newMap = map.filter { case (_, v) => !v.isBottom }
      if (newMap.isEmpty) Bot else Elem(newMap)
}
case object RecordTopTy extends RecordTy
case class RecordElemTy(map: Map[String, ValueTy] = Map()) extends RecordTy
object RecordTy extends Parser.From(Parser.recordTy) {
  def apply(fields: Set[String]): RecordTy =
    apply(fields.map(_ -> ValueTy.Top).toMap)
  def apply(fields: Map[String, ValueTy]): RecordTy = Elem(fields).normalized
  val Elem = RecordElemTy
  type Elem = RecordElemTy
  lazy val Top = RecordTopTy
  lazy val Bot = Elem()
}
