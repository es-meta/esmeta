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
      case (_, Top)                 => true
      case (Top, _)                 => false
      case (Elem(lmap), Elem(rmap)) => isSubTy(lmap, rmap)
  }

  /** union type */
  def ||(that: => RecordTy): RecordTy = (this, that) match
    case _ if this eq that   => this
    case (Bot, _) | (_, Top) => that
    case (Top, _) | (_, Bot) => this
    case (Elem(lmap), Elem(rmap)) =>
      var map = lmap.filter(!isStrictSubTy(_, rmap))
      for {
        (t, fm) <- rmap.filter(!isStrictSubTy(_, lmap))
      } map += t -> map.get(t).fold(fm)(_ || fm)
      Elem(map)

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
          ls.filter(isSubTy(_, rs)) ++
          rs.filter(isSubTy(_, ls))
        }
        fm = {
          lmap.getOrElse(t, FieldMap.Top) &&
          rmap.getOrElse(t, FieldMap.Top)
        }
        pair <- normalize(t -> fm)
      } yield pair).toMap)

  /** prune type */
  def --(that: => RecordTy): RecordTy = (this, that) match
    case (Bot, _) | (_, Top) => Bot
    case (Top, _) | (_, Bot) => this
    case (Elem(lmap), Elem(rmap)) =>
      Elem(lmap.filter { (l, lfm) =>
        !rmap.exists { (r, rfm) =>
          isStrictSubTy(l, r) || (l == r && lfm <= rfm)
        }
      })

  /** field type map */
  def fieldMap: Option[FieldMap] = this match
    case Top       => Some(FieldMap.Top)
    case Elem(map) => map.map(getAllFieldMap(_) && _).reduceOption(_ || _)

  /** base type names */
  def bases: BSet[String] = this match
    case Top       => Inf
    case Elem(map) => Fin(map.keySet.map(getBase))

  /** type names */
  def names: BSet[String] = this match
    case Top       => Inf
    case Elem(map) => Fin(map.keySet)

  /** field accessor */
  def apply(f: String): OptValueTy = this match
    case Top       => MayAnyT
    case Elem(map) => map.map(getField(_, f) && _(f)).foldLeft(MustBotT)(_ || _)

  /** field accessor for specific record type */
  def apply(name: String, f: String): OptValueTy = this match
    case Top       => MayAnyT
    case Elem(map) => map.get(name).fold(MustBotT)(getField(name, f) && _(f))

  /** field update */
  def update(field: String, ty: ValueTy): RecordTy = this match
    case Top       => Top
    case Elem(map) => Elem(map.map { _ -> _.update(field, ty) })

  /** record containment check */
  def contains(record: RecordObj, heap: Heap): Boolean = this match
    case Top => true
    case Elem(map) =>
      val RecordObj(l, lfm) = record
      map.exists { (r, rfm) =>
        isStrictSubTy(l, r) ||
        (l == r && rfm.contains(record, heap)) ||
        (for {
          lca <- getLCA(l, r)
          fm <- getDiffFieldMap(lca, r)
        } yield fm && rfm).exists(_.contains(record, heap))
      }

  /** filter with possible field types */
  def filter(field: String, ty: OptValueTy): RecordTy = this match
    case Top => Top
    case Elem(map) =>
      Elem(map.filter {
        case (name, _) => !(apply(name, field) && ty).isBottom
      })
}

object RecordTy extends Parser.From(Parser.recordTy) {
  import ManualInfo.tyModel.*

  lazy val Bot: RecordTy = Elem(Map.empty)

  def apply(names: String*): RecordTy =
    apply(names.toSet)
  def apply(names: Set[String]): RecordTy =
    apply(names.toList.map(_ -> FieldMap.Top).toMap)
  def apply(name: String, fields: Map[String, ValueTy]): RecordTy =
    apply(Map(name -> FieldMap(fields.map(_ -> OptValueTy(_, false)).toMap)))
  def apply(name: String, fieldMap: FieldMap): RecordTy =
    apply(Map(name -> fieldMap))
  def apply(map: Map[String, FieldMap]): RecordTy =
    Elem(map)

  /** normalized type */
  def normalize(pair: (String, FieldMap)): Map[String, FieldMap] =
    val (l, lfm @ FieldMap(lm)) = pair
    val pairs = for {
      r <- getDirectSubTys(l).toList
      dfm <- getDiffFieldMap(l, r)
      if lfm <= dfm
      fm = FieldMap(lfm.map.filter { case (field, ty) => dfm(field) != ty })
    } yield r -> fm
    if (pairs.isEmpty) Map(l -> lfm)
    else pairs.toMap
}
