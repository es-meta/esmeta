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
      def aux(
        lmap: Map[String, FieldMap],
        rmap: Map[String, FieldMap],
      ): List[(String, FieldMap)] = lmap.toList.map { (t, fm) =>
        rmap.find((u, _) => isSubTy(t, u)) match
          case Some((u, ufm)) =>
            u -> FieldMap(ufm.map.map((f, _) => f -> get((t, fm), f)))
          case None => t -> fm
      }
      val lpairs = aux(lmap, rmap)
      val rpairs = aux(rmap, lmap)
      Elem((lpairs ++ rpairs).foldLeft(Map[String, FieldMap]()) {
        case (map, (t, fm)) => map + (t -> map.get(t).fold(fm)(_ || fm))
      }).normalized

  /** intersection type */
  def &&(that: => RecordTy): RecordTy = (this, that) match
    case _ if this eq that   => this
    case (Bot, _) | (_, Top) => this
    case (Top, _) | (_, Bot) => that
    case (Elem(lm), Elem(rm)) =>
      val ls = lm.keySet
      val rs = rm.keySet
      val lmap = for { (t, fm) <- lm } yield {
        if (isSubTy(t, rs)) t -> fm
        else normalizedOf(t).fold(t -> fm)((u, ufm) => u -> (ufm && fm))
      }
      val rmap = for { (t, fm) <- rm } yield {
        if (isSubTy(t, ls)) t -> fm
        else normalizedOf(t).fold(t -> fm)((u, ufm) => u -> (ufm && fm))
      }
      val lns = lmap.keySet
      val rns = rmap.keySet
      Elem(
        (for {
          t <- lns.filter(isSubTy(_, rns)) ++ rns.filter(isSubTy(_, lns))
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
    case Elem(map) => map.map(get(_, f)).foldLeft(Binding.Bot)(_ || _)

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
          map ++ (for {
            (t, fm) <- RecordTy.update(pair, field, elem, refine)
          } yield t -> map.get(t).fold(fm)(_ || fm))
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
    apply(
      fields.foldLeft(Map(name -> FieldMap.Top)) {
        case (map, (f, ty)) =>
          for {
            pair <- map
            pair <- update(pair, f, Binding(ty), refine = false)
          } yield pair
      },
    )
  def apply(pair: (String, FieldMap)): RecordTy = apply(Map(pair))
  def apply(name: String, fieldMap: FieldMap): RecordTy =
    apply(Map(name -> fieldMap))
  def apply(map: Map[String, FieldMap]): RecordTy =
    Elem(map)

  /** field accessor for specific record type */
  private def get(pair: (String, FieldMap), f: String): Binding =
    val (t, fm) = pair
    getField(t, f) && fm(f)

  /** field update */
  private def update(
    t: String,
    fm: FieldMap,
    refine: Boolean,
  ): Map[String, FieldMap] =
    fm.map.foldLeft(Map(t -> FieldMap.Top)) {
      case (map, (f, binding)) =>
        for {
          pair <- map
          pair <- update(pair, f, binding, refine)
        } yield pair
    }

  /** field update */
  private def update(
    pair: (String, FieldMap),
    field: String,
    bind: Binding,
    refine: Boolean,
  ): Map[String, FieldMap] =
    val (t, fm) = pair
    val existCheck = bind == Binding.Exist
    val newBind = getField(t, field) && bind
    if (newBind.isBottom)
      if (!refine && !(bind <= getField(baseOf(t), field))) Map(pair)
      else Map()
    else
      val existBased = for {
        map <- existRefinerOf.get(t)
        u <- map.get(field)
        if existCheck
      } yield Set(u)
      val set = existBased.getOrElse(
        for {
          map <- refinerOf(t).get(field).toSet
          (_, u) <- map.filter { (ty, _) =>
            existCheck || (newBind <= Binding(ty))
          }
        } yield u,
      ) + t
      val xs = set.toList.filter(x => !set.exists(y => isStrictSubTy(y, x)))
      val refined = if (refine) get(pair, field) && bind else newBind
      if (refined.isBottom) Map()
      else xs.map(x => normalize(x -> fm.update(field, refined))).toMap

  /** normalized type */
  private def normalize(pair: (String, FieldMap)): (String, FieldMap) =
    val (t, fm) = pair
    t -> FieldMap(fm.map.filter { (f, elem) => !(getField(t, f) <= elem) })
}
