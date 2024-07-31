package esmeta.ty

import esmeta.util.*
import esmeta.state.{Value, RecordObj, Heap}
import esmeta.ty.util.Parser

/** record types */
enum RecordTy extends TyElem with Lattice[RecordTy] {

  case Top

  /** a record type with a named record types and refined fields */
  case Elem(map: Map[String, FieldMap])

  import ManualInfo.tyModel.*
  import RecordTy.*

  /** top check */
  def isTop: Boolean = this == Top

  /** bottom check */
  def isBottom: Boolean = this == Bot

  /** partial order/subset operator */
  def <=(that: => RecordTy): Boolean = (this eq that) || {
    (this, that) match
      case (_, Top) => true
      case (Top, _) => false
      case (Elem(lmap), Elem(rmap)) =>
        lmap.forall { (l, lfm) =>
          rmap.exists { (r, rfm) =>
            isStrictSubTy(l, r) || (l == r && lfm <= rfm)
          }
        }
  }

  /** union type */
  def ||(that: => RecordTy): RecordTy = (this, that) match
    case _ if this eq that   => this
    case (Bot, _) | (_, Top) => that
    case (Top, _) | (_, Bot) => this
    case (Elem(lmap), Elem(rmap)) =>
      val ls = lmap.keySet
      val rs = rmap.keySet
      Elem((for {
        t <- {
          ls.filter(!isStrictSubTy(_, rs)) ++
          rs.filter(!isStrictSubTy(_, ls))
        }
        fm = {
          lmap.getOrElse(t, FieldMap.Top) ||
          rmap.getOrElse(t, FieldMap.Top)
        }
      } yield t -> fm).toMap)

  /** intersection type */
  def &&(that: => RecordTy): RecordTy = (this, that) match
    case _ if this eq that   => this
    case (Bot, _) | (_, Top) => this
    case (Top, _) | (_, Bot) => that
    case (Elem(lmap), Elem(rmap)) =>
      val ls = lmap.keySet
      val rs = rmap.keySet
      Elem((for {
        t <- {
          ls.filter(isStrictSubTy(_, rs)) ++
          rs.filter(isStrictSubTy(_, ls))
        }
        fm = {
          lmap.getOrElse(t, FieldMap.Top) &&
          rmap.getOrElse(t, FieldMap.Top)
        }
      } yield t -> fm).toMap)

  /** prune type */
  def --(that: => RecordTy): RecordTy = (this, that) match
    case (Bot, _) | (_, Top) => Bot
    case (Top, _) | (_, Bot) => this
    case (Elem(lmap), Elem(rmap)) =>
      Elem(lmap.filter { (l, lfm) =>
        rmap.exists { (r, rfm) =>
          !(isStrictSubTy(l, r) || (l == r && lfm <= rfm))
        }
      })

  /** field type map */
  def fieldMap: Option[FieldMap] = this match
    case Top       => Some(FieldMap.Top)
    case Elem(map) => map.map(getFieldMap(_) && _).reduceOption(_ || _)

  /** base type names */
  def bases: BSet[String] = this match
    case Top       => Inf
    case Elem(map) => Fin(map.keySet.map(getBase))

  /** type names */
  def names: BSet[String] = this match
    case Top       => Inf
    case Elem(map) => Fin(map.keySet)

  /** field accessor */
  def apply(f: String): ValueTy = this match
    case Top       => TopT
    case Elem(map) => map.map(getField(_, f) && _(f)).foldLeft(BotT)(_ || _)

  /** record containment check */
  def contains(record: RecordObj, heap: Heap): Boolean = this match
    case Top => true
    case Elem(map) =>
      val RecordObj(l, lfm) = record
      map.exists { (r, rfm) =>
        isStrictSubTy(l, r) ||
        (l == r && rfm.contains(record, heap)) ||
        (getLCA(l, r).exists { getDiffFieldMap(r, _).contains(record, heap) })
      }

  /** normalized type */
  def normalized: RecordTy = this match
    case Top => Top
    case Elem(map) =>
      Elem(map.filter { (l, lfm) =>
        !map.exists { (r, rfm) =>
          l != r && (isStrictSubTy(l, r) || (l == r && lfm <= rfm))
        }
      })
}

object RecordTy extends Parser.From(Parser.recordTy) {
  lazy val Bot: RecordTy = Elem(Map.empty)

  def apply(names: String*): RecordTy =
    apply(names.toSet)
  def apply(names: Set[String]): RecordTy =
    apply(names.toList.map(_ -> FieldMap.Top).toMap)
  def apply(name: String, fields: Map[String, ValueTy]): RecordTy =
    apply(Map(name -> FieldMap(fields)))
  def apply(map: Map[String, FieldMap]): RecordTy =
    Elem(map).normalized
}
