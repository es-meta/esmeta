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
        pair = t -> fm
        // TODO pair <- normalize(t -> fm)
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
  def update(field: String, ty: ValueTy): RecordTy = this match
    case Top       => Top
    case Elem(map) => Elem(map.map { _ -> _.update(field, FMElem(ty)) })

  /** field update */
  def expand(field: String): RecordTy = this match
    case Top       => Top
    case Elem(map) => Elem(map.map { _ -> _.update(field, FMElem.Uninit) })

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

  /** prune with possible field types */
  def pruneField(f: String, ty: FMElem, pos: Boolean): RecordTy = this match
    case Top => Top
    case Elem(map) =>
      (for {
        (name, fm) <- map
        subs = tyModel.pruneField(name, f)
        sub <- if (pos) subs else if (subs.isEmpty) Set() else Set(name)
        vty = getField(sub, f) && fm(f)
        rty = if (pos) vty && ty else vty -- ty
        if !rty.isBottom
        pair = sub -> fm.update(f, rty)
        // TODO pair <- normalize(sub -> fm.update(f, rty))
      } yield Elem(Map(pair))).foldLeft(Bot)(_ || _)

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
  def apply(name: String, fields: Map[String, ValueTy]): RecordTy = apply(
    Map(
      name -> FieldMap(
        map = (for {
          (field, ty) <- fields
          elem = FMElem(ty, false, false)
          if isValidField(name, field, elem)
        } yield field -> elem).toMap,
        default = FMElem.Top,
      ),
    ),
  )
  def apply(name: String, fieldMap: FieldMap): RecordTy =
    apply(Map(name -> fieldMap))
  def apply(map: Map[String, FieldMap]): RecordTy =
    Elem(map)

  /** normalized type */
  def normalize(pair: (String, FieldMap)): (String, FieldMap) =
    val (t, fm) = pair
    t -> fm.filter(f => isValidField(t, f, fm(f)))

  /** normalized type */
  def isValidField(t: String, field: String, elem: FMElem): Boolean =
    !(getField(t, field) <= elem)

  /** normalized type */
  // def normalize(pair: (String, FieldMap)): Map[String, FieldMap] =
  //   val (l, lfm @ FieldMap(lm, default)) = pair
  //   val pairs = for {
  //     r <- directSubTysOf(l).toList
  //     dfm <- diffOf(l, r)
  //     if lfm <= dfm
  //     fm = FieldMap(
  //       lfm.map.filter { case (field, ty) => dfm(field) != ty },
  //       default,
  //     )
  //   } yield r -> fm
  //   val map =
  //     if (pairs.isEmpty) Map(l -> lfm)
  //     else pairs.toMap
  //   map.map { case (r, fm) => r -> fm.filter(f => fm(f) != getField(r, f)) }
}
