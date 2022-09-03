package esmeta.ty

import esmeta.util.*
import esmeta.ty.util.Parser

/** record types */
case class RecordTy(
  map: Map[String, Option[ValueTy]] = Map(),
) extends TyElem
  with Lattice[RecordTy] {

  /** bottom check */
  def isBottom: Boolean = this.map.isEmpty

  /** partial order/subset operator */
  def <=(that: => RecordTy): Boolean = (for {
    field <- (this.map.keySet | that.map.keySet).toList
    bool = (this.map.get(field), that.map.get(field)) match
      case (None, _) | (_, Some(None))    => true
      case (_, None) | (Some(None), _)    => false
      case (Some(Some(l)), Some(Some(r))) => l <= r
  } yield bool).forall(_ == true)

  /** union type */
  def |(that: => RecordTy): RecordTy = RecordTy((for {
    field <- (this.map.keySet | that.map.keySet).toList
    value <- (this.map.get(field), that.map.get(field)) match
      case (None, r)                         => r
      case (l, None)                         => l
      case (Some(None), _) | (_, Some(None)) => Some(None)
      case (Some(Some(l)), Some(Some(r)))    => Some(Some(l | r))
  } yield field -> value).toMap)

  /** intersection type */
  def &(that: => RecordTy): RecordTy = RecordTy((for {
    field <- (this.map.keySet | that.map.keySet).toList
    value <- (this.map.get(field), that.map.get(field)) match
      case (Some(None), r)                => r
      case (l, Some(None))                => l
      case (None, _) | (_, None)          => None
      case (Some(Some(l)), Some(Some(r))) => Some(Some(l & r))
  } yield field -> value).toMap).norm

  /** prune type */
  def --(that: => RecordTy): RecordTy = RecordTy((for {
    field <- (this.map.keySet | that.map.keySet).toList
    value <- (this.map.get(field), that.map.get(field)) match
      case (None, _) | (_, Some(None))    => None
      case (Some(None), _)                => Some(None)
      case (l, None)                      => l
      case (Some(Some(l)), Some(Some(r))) => Some(Some(l -- r))
  } yield field -> value).toMap).norm

  // normalization
  def norm: RecordTy = RecordTy(
    this.map.filter { case (_, v) => v.fold(true)(!_.isBottom) },
  )
}
object RecordTy extends Parser.From(Parser.recordTy):
  def apply(fields: Set[String]): RecordTy =
    RecordTy(fields.map(x => x -> None).toMap)
