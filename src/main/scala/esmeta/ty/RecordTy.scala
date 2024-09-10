package esmeta.ty

import esmeta.util.*
import esmeta.state.{Value, RecordObj, Heap}
import esmeta.ty.util.Parser
import FieldMap.{Elem => FMElem}

/** record types */
enum RecordTy extends TyElem with Lattice[RecordTy] {

  case Top

  /** a record type with a named record types and refined fields */
  case Elem(map: Map[String, FieldMap])

  import ManualInfo.tyModel
  import tyModel.*
  import RecordTy.*

  /** top check */
  def isTop: Boolean = this == Top

  /** bottom check */
  def isBottom: Boolean = this == Bot

  /** partial order/subset operator */
  def <=(that: => RecordTy): Boolean = (this eq that) || {
    (this, that) match
      case (Bot, _) | (_, Top)      => true
      case (Top, _) | (_, Bot)      => false
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
      Elem(map).normalized

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
        pair = update(t, fm)
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

  /** get key type */
  def getKey: ValueTy = this match
    case Top => StrT
    case Elem(map) =>
      StrT((for {
        (name, fm) <- map.toList
        f <- fm.map.keySet ++ fieldMapOf(name).map.keySet
      } yield f).toSet)

  /** field type map */
  def fieldMap: Option[FieldMap] = this match
    case Top       => Some(FieldMap.Top)
    case Elem(map) => map.map(fieldMapOf(_) && _).reduceOption(_ || _)

  /** base type names */
  def bases: BSet[String] = this match
    case Top       => Inf
    case Elem(map) => Fin(map.keySet.map(baseOf))

  /** type names */
  def names: BSet[String] = this match
    case Top       => Inf
    case Elem(map) => Fin(map.keySet)

  /** field accessor */
  def apply(f: String): FMElem = this match
    case Top => FMElem.Top
    case Elem(map) =>
      map.map(getField(_, f) && _(f)).foldLeft(FMElem.Bot)(_ || _)

  /** field accessor for specific record type */
  def apply(name: String, f: String): FMElem = this match
    case Top => FMElem.Top
    case Elem(map) =>
      map.get(name).fold(FMElem.Bot)(getField(name, f) && _(f))

  /** field update */
  def update(field: String, ty: ValueTy): RecordTy = update(field, FMElem(ty))

  /** field update */
  def update(field: String, elem: FMElem): RecordTy = this match
    case Top => Top
    case Elem(map) =>
      Elem(map.foldLeft(Map[String, FieldMap]()) {
        case (map, pair) =>
          val (t, fm) = update(pair, field, elem)
          map + (t -> map.get(t).fold(fm)(_ || fm))
      })

  /** field update */
  private def update(t: String, fm: FieldMap): (String, FieldMap) =
    fm.map.foldLeft(t -> FieldMap.Top) {
      case (pair, (f, elem)) => update(pair, f, elem)
    }

  /** field update */
  private def update(
    pair: (String, FieldMap),
    field: String,
    elem: FMElem,
  ): (String, FieldMap) =
    val (t, fm) = pair
    val refined = (for {
      map <- refinerOf(t).get(field)
      (_, u) <- map.find { case (e, _) => elem <= e }
    } yield u).getOrElse(t)
    normalize(refined -> fm.update(field, elem))

  /** record containment check */
  def contains(record: RecordObj, heap: Heap): Boolean = this match
    case Top => true
    case Elem(map) =>
      val RecordObj(l, lfm) = record
      map.exists { (r, rfm) =>
        isStrictSubTy(l, r) ||
        (l == r && rfm.contains(record, heap)) ||
        (for {
          lca <- lcaOf(l, r)
          fm <- diffOf(lca, r)
        } yield fm && rfm).exists(_.contains(record, heap))
      }

  /** normalize record type */
  def normalized: RecordTy = this match
    case Top       => Top
    case Elem(map) => Elem(map.map(normalize))

  /** normalized type */
  private def normalize(pair: (String, FieldMap)): (String, FieldMap) =
    val (t, fm) = pair
    t -> FieldMap(fm.map.filter { (f, elem) => isValidField(t, f, elem) })

  /** normalized type */
  private def isValidField(t: String, field: String, elem: FMElem): Boolean =
    !(getField(t, field) <= elem)
}

object RecordTy extends Parser.From(Parser.recordTy) {
  import ManualInfo.tyModel.*

  lazy val Bot: RecordTy = Elem(Map.empty)

  def apply(names: String*): RecordTy =
    apply(names.toSet)
  def apply(names: Set[String]): RecordTy =
    apply(names.toList.map(_ -> FieldMap.Top).toMap)
  def apply(name: String, fields: Map[String, ValueTy]): RecordTy = apply(
    Map(
      name -> FieldMap(
        (for {
          (field, ty) <- fields
          elem = FMElem(ty, false, false)
        } yield field -> elem).toMap,
      ),
    ),
  ).normalized
  def apply(name: String, fieldMap: FieldMap): RecordTy =
    apply(Map(name -> fieldMap))
  def apply(map: Map[String, FieldMap]): RecordTy =
    Elem(map)
}
