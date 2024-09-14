package esmeta.ty

import esmeta.util.*
import esmeta.state.{Value, RecordObj, Heap}
import esmeta.ty.util.Parser

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
      Elem(
        (for {
          t <- ls.filter(isSubTy(_, rs)) ++ rs.filter(isSubTy(_, ls))
          lfm = lmap.getOrElse(t, FieldMap.Top)
          rfm = rmap.getOrElse(t, FieldMap.Top)
          fm = lfm && rfm
          pair <- RecordTy.update(t, fm, refine = true)
        } yield pair)
          .groupBy(_._1)
          .map { case (t, pairs) => t -> pairs.map(_._2).reduce(_ && _) }
          .toMap,
      )

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
        f <- fm.map.keySet ++ fieldsOf(name).keySet
      } yield f).toSet)

  /** base type names */
  def bases: BSet[String] = this match
    case Top       => Inf
    case Elem(map) => Fin(map.keySet.map(baseOf))

  /** type names */
  def names: BSet[String] = this match
    case Top       => Inf
    case Elem(map) => Fin(map.keySet)

  /** field accessor */
  def apply(f: String): Binding = this match
    case Top       => Binding.Top
    case Elem(map) => map.map(RecordTy(_, f)).foldLeft(Binding.Bot)(_ || _)

  /** field update */
  def update(field: String, ty: ValueTy, refine: Boolean): RecordTy =
    update(field, Binding(ty), refine)

  /** field update */
  def update(
    field: String,
    elem: Binding,
    refine: Boolean,
  ): RecordTy = this match
    case Top => Top
    case Elem(map) =>
      Elem(map.foldLeft(Map[String, FieldMap]()) {
        case (map, pair) =>
          RecordTy.update(pair, field, elem, refine).fold(map) { (t, fm) =>
            map + (t -> map.get(t).fold(fm)(_ || fm))
          }
      })

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
}

object RecordTy extends Parser.From(Parser.recordTy) {
  import ManualInfo.tyModel.*

  lazy val Bot: RecordTy = Elem(Map.empty)

  def apply(names: String*): RecordTy =
    apply(names.toSet)
  def apply(names: Set[String]): RecordTy =
    apply(names.toList.map(_ -> FieldMap.Top).toMap)
  def apply(name: String, fields: Map[String, ValueTy]): RecordTy =
    fields
      .foldLeft(Option(name -> FieldMap.Top)) {
        case (None, _) => None
        case (Some(pair), (f, ty)) =>
          update(pair, f, Binding(ty), refine = false)
      }
      .fold(Bot)(apply)
  def apply(pair: (String, FieldMap)): RecordTy = apply(Map(pair))
  def apply(name: String, fieldMap: FieldMap): RecordTy =
    apply(Map(name -> fieldMap))
  def apply(map: Map[String, FieldMap]): RecordTy =
    Elem(map)

  /** field accessor for specific record type */
  private def apply(pair: (String, FieldMap), f: String): Binding =
    val (t, fm) = pair
    getField(t, f) && fm(f)

  /** field update */
  private def update(
    t: String,
    fm: FieldMap,
    refine: Boolean,
  ): Option[(String, FieldMap)] =
    fm.map.foldLeft(Option(t -> FieldMap.Top)) {
      case (None, _)               => None
      case (Some(pair), (f, elem)) => update(pair, f, elem, refine)
    }

  /** field update */
  private def update(
    pair: (String, FieldMap),
    field: String,
    elem: Binding,
    refine: Boolean,
  ): Option[(String, FieldMap)] =
    val (t, fm) = pair
    val x = (for {
      map <- refinerOf(t).get(field)
      (_, u) <- map.find { case (e, _) => elem <= e }
    } yield u).getOrElse(t)
    if (refine)
      val refined = elem && apply(pair, field)
      if (refined.isBottom) None
      else Some(normalize(x -> fm.update(field, refined)))
    else Some(normalize(x -> fm.update(field, elem)))

  /** normalized type */
  private def normalize(pair: (String, FieldMap)): (String, FieldMap) =
    val (t, fm) = pair
    t -> FieldMap(fm.map.filter { (f, elem) => !(getField(t, f) <= elem) })
}
