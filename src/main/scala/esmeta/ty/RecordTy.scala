package esmeta.ty

import esmeta.util.*
import esmeta.state.{Value, RecordObj, Heap}
import esmeta.ty.util.Parser

/** record types */
enum RecordTy extends TyElem with Lattice[RecordTy] {

  case Top

  /** a record type with a named record types and refined fields */
  case Elem(
    map: Map[String, FieldMap] = Map.empty,
    obj: ObjShape = ObjShape.Top,
  )

  import ManualInfo.tyModel
  import tyModel.*
  import RecordTy.*

  /** top check */
  def isTop: Boolean = this == Top

  /** bottom check */
  def isBottom: Boolean = this == Bot

  /** partial order/subset operator */
  def <=(that: => RecordTy): Boolean = (this == that) || {
    (this, that) match
      case (Bot, _) | (_, Top) => true
      case (Top, _) | (_, Bot) => false
      case (Elem(lmap, lobj), Elem(rmap, robj)) =>
        isSubTy(lmap, rmap) && lobj <= robj
  }

  /** union type */
  def ||(that: => RecordTy): RecordTy = (this, that) match
    case _ if this == that   => this
    case (Bot, _) | (_, Top) => that
    case (Top, _) | (_, Bot) => this
    case (Elem(lmap, lobj), Elem(rmap, robj)) =>
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
      val map = (lpairs ++ rpairs).foldLeft(Map[String, FieldMap]()) {
        case (map, (t, fm)) => map + (t -> map.get(t).fold(fm)(_ || fm))
      }
      Elem(map, lobj || robj).normalized

  /** intersection type */
  def &&(that: => RecordTy): RecordTy = (this, that) match
    case _ if this == that   => this
    case (Bot, _) | (_, Top) => this
    case (Top, _) | (_, Bot) => that
    case (Elem(lm, lobj), Elem(rm, robj)) =>
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
      val map = (for {
        t <- lmap.keySet ++ rmap.keySet
        ancestors = ancestorsOf(t)
        (l, lfm) <- ancestors.filter(lmap.contains).map(l => l -> lmap(l))
        (r, rfm) <- ancestors.filter(rmap.contains).map(r => r -> rmap(r))
        fm = lfm && rfm
        pair <- RecordTy.update(t, fm, refine = true)
      } yield pair)
        .groupBy(_._1)
        .map { case (t, pairs) => t -> pairs.map(_._2).reduce(_ && _) }
        .toMap
      Elem(map, lobj && robj).normalized

  /** prune type */
  def --(that: => RecordTy): RecordTy = (this, that) match
    case (Bot, _) | (_, Top) => Bot
    case (Top, _) | (_, Bot) => this
    case (Elem(lmap, lprops), Elem(rmap, _)) =>
      val map = lmap.filter { (l, lfm) =>
        !rmap.exists { (r, rfm) =>
          isStrictSubTy(l, r) || (l == r && lfm <= rfm)
        }
      }
      Elem(map, lprops).normalized

  /** get key type */
  def getKey: ValueTy = this match
    case Top => StrT
    case Elem(map, _) =>
      StrT((for {
        (name, fm) <- map.toList
        f <- fm.map.keySet ++ fieldsOf(name).keySet
      } yield f).toSet)

  /** base type names */
  def bases: BSet[String] = this match
    case Top          => Inf
    case Elem(map, _) => Fin(map.keySet.map(baseOf))

  /** type names */
  def names: BSet[String] = this match
    case Top          => Inf
    case Elem(map, _) => Fin(map.keySet)

  /** field accessor */
  def apply(f: String): Binding = this match
    case Top          => Binding.Top
    case Elem(map, _) => map.map(get(_, f)).foldLeft(Binding.Bot)(_ || _)

  /** property accessor */
  def apply(p: Property): Desc = this match
    case Top          => Desc.Top
    case Elem(_, obj) => obj(p)

  /** call accessor */
  def call: CallDesc = this match
    case Top          => CallDesc.Top
    case Elem(_, obj) => obj.call

  /** construct accessor */
  def construct: ConstructDesc = this match
    case Top          => ConstructDesc.Top
    case Elem(_, obj) => obj.construct

  /** property update */
  def update(p: Property, desc: Desc): RecordTy = this match
    case Top            => Top
    case Elem(map, obj) => Elem(map, obj + (p -> desc))

  /** function call return update */
  def update(call: CallDesc): RecordTy = this match
    case Top            => Top
    case Elem(map, obj) => Elem(map, obj.copy(call = call))

  /** construct return update */
  def update(construct: ConstructDesc): RecordTy = this match
    case Top            => Top
    case Elem(map, obj) => Elem(map, obj.copy(construct = construct))

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
    case Elem(map, obj) =>
      Elem(
        map.foldLeft(Map[String, FieldMap]()) {
          case (map, pair) =>
            map ++ (for {
              (t, fm) <- RecordTy.update(pair, field, elem, refine)
            } yield t -> map.get(t).fold(fm)(_ || fm))
        },
        obj,
      ).boundFields(maxFieldDepth)

  /** bound fields */
  def boundFields(depth: Int): RecordTy = this match
    case Top => Top
    case Elem(map, obj) =>
      Elem(
        map.map { (t, fm) =>
          t -> {
            if (depth == 0) FieldMap.Top
            else
              FieldMap(fm.map.map { (f, elem) =>
                f -> elem.copy(value = elem.value match
                  case ValueTopTy => ValueTopTy
                  case v: ValueElemTy =>
                    v.copy(record = v.record.boundFields(depth - 1)),
                )
              })
          }
        },
        obj,
      )

  /** record containment check (ignoring properties) */
  def contains(record: RecordObj, heap: Heap): Boolean = this match
    case Top => true
    case Elem(map, _) =>
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
    case Top => Top
    case Elem(map, obj) =>
      val m = map.map(normalize)
      if (m.exists((x, _) => isSubTy(x, "Object"))) Elem(m, obj)
      else Elem(m)

  /** to list of atomic record types */
  def toAtomicTys: List[RecordTy] = this match
    case Top          => List(Top)
    case Elem(map, _) => map.toList.map { (t, fm) => Elem(Map(t -> fm)) }
}

object RecordTy extends Parser.From(Parser.recordTy) {
  import ManualInfo.tyModel.*

  lazy val Bot: RecordTy = Elem()
  lazy val Object: RecordTy = apply("Object")

  lazy val maxFieldDepth: Int = 4

  def apply(names: String*): RecordTy =
    apply(names.toSet)
  def apply(names: Set[String]): RecordTy =
    apply(names.toList.map(_ -> FieldMap.Top).toMap)
  def apply(name: String, fields: List[String]): RecordTy =
    apply(name, fields.map(_ -> AnyT).toMap)
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
  def apply(map: Map[String, FieldMap]): RecordTy = Elem(map)
  def apply(map: Map[String, FieldMap], obj: ObjShape): RecordTy =
    Elem(map, obj)

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
  ): Map[String, FieldMap] = {
    val (t, fm) = pair
    val existCheck = bind == Binding.Exist
    val refined = bind && (if (refine) get(pair, field) else getField(t, field))
    if (refined.isBottom)
      // TODO check why this is needed and remove it if possible
      if (!refine && !(bind <= getField(baseOf(t), field))) Map(pair)
      else Map()
    else
      var newFM = fm + (field -> refined)
      getPropRefiner(field) match
        case Some(fs) if !refined.absent =>
          for (f <- fs) newFM += f -> (get(pair, f) && Binding.Exist)
          Map(normalize(t -> newFM))
        case Some(_) => Map(normalize(t -> newFM))
        case None =>
          val set = (
            for {
              map <- refinerOf(t).get(field).toSet
              (_, u) <- map.filter { (ty, _) =>
                existCheck || (refined <= Binding(ty))
              }
            } yield u,
          ) + t
          val xs = set.toList.filter(x => !set.exists(y => isStrictSubTy(y, x)))
          xs.map(x => normalize(x -> newFM)).toMap
  }

  /** normalized type */
  private def normalize(pair: (String, FieldMap)): (String, FieldMap) =
    val (t, fm) = pair
    t -> FieldMap(fm.map.flatMap { (f, elem) =>
      val orig = getField(t, f)
      if (orig <= elem) None
      else if (orig.value <= elem.value) Some(f -> elem.copy(value = AnyT))
      else Some(f -> elem)
    })
}

case class ObjShape(
  props: Map[Property, Desc] = Map.empty,
  call: CallDesc = CallDesc.Top,
  construct: ConstructDesc = ConstructDesc.Top,
) extends TyElem {
  import ObjShape.*
  def isBottom: Boolean =
    props.exists((_, desc) => desc.isBottom) ||
    call.isBottom || construct.isBottom
  def <=(that: ObjShape): Boolean =
    that.props.forall { (p, d) => this.props.get(p).fold(false)(_ <= d) } &&
    this.call <= that.call && this.construct <= that.construct
  def ||(that: ObjShape): ObjShape = ObjShape(
    props = (for {
      p <- (this.props.keySet intersect that.props.keySet).toList
    } yield p -> (this(p) || that(p))).toMap,
    call = this.call || that.call,
    construct = this.construct || that.construct,
  )
  def &&(that: ObjShape): ObjShape = ObjShape(
    props = (for {
      p <- (this.props.keySet ++ that.props.keySet).toList
    } yield p -> (this(p) && that(p))).toMap,
    call = this.call && that.call,
    construct = this.construct && that.construct,
  )
  def +(pair: (Property, Desc)): ObjShape =
    ObjShape(props + pair, call, construct)

  def apply(p: Property): Desc = props.getOrElse(p, Desc.Top)
}
object ObjShape {
  lazy val Top: ObjShape = ObjShape()
}

enum Property extends TyElem:
  case PStr(str: String)
  case PSym(sym: String)

case class Desc(
  getExc: Boolean = false,
  setExc: Boolean = false,
  ty: ValueTy = BotT,
) extends TyElem {
  def isBottom: Boolean = !getExc && !setExc && ty.isBottom
  def isTop: Boolean = getExc && setExc && ty.isTop
  def <=(that: Desc): Boolean =
    (this.getExc <= that.getExc) &&
    (this.setExc <= that.setExc) &&
    (this.ty <= that.ty)
  def &&(that: Desc): Desc = Desc(
    this.getExc && that.getExc,
    this.setExc && that.setExc,
    this.ty && that.ty,
  )
  def ||(that: Desc): Desc = Desc(
    this.getExc || that.getExc,
    this.setExc || that.setExc,
    this.ty || that.ty,
  )
  def getTy: ValueTy = NormalT(ty) || (if (getExc) ThrowT else BotT)
}
object Desc {
  val Bot: Desc = Desc()
  val Top: Desc = Desc(getExc = true, setExc = true, ESValueT)
  val GetExc: Desc = Desc(getExc = true)
  val SetExc: Desc = Desc(setExc = true)
}

enum CallDesc extends TyElem {
  case Top
  case Elem(exc: Boolean = false, ret: ValueTy = BotT)
  def isBottom: Boolean = this match
    case Top            => false
    case Elem(exc, ret) => !exc && ret.isBottom
  def exists: Boolean = this != Top
  def <=(that: CallDesc): Boolean = (this, that) match
    case (_, Top)                     => true
    case (Top, _)                     => false
    case (Elem(le, lr), Elem(re, rr)) => (le <= re) && (lr <= rr)
  def ||(that: CallDesc): CallDesc = (this, that) match
    case (Top, _) | (_, Top)          => Top
    case (Elem(le, lr), Elem(re, rr)) => Elem(le || re, lr || rr)
  def &&(that: CallDesc): CallDesc = (this, that) match
    case (_, Top)                     => this
    case (Top, _)                     => that
    case (Elem(le, lr), Elem(re, rr)) => Elem(le && re, lr && rr)
  def getTy: ValueTy = this match
    case Top            => NormalT(ESValueT) || ThrowT
    case Elem(exc, ret) => NormalT(ret) || (if (exc) ThrowT else BotT)
}
object CallDesc {
  lazy val Bot: CallDesc = Elem()
  lazy val Exist: CallDesc = Elem(exc = true, ret = ESValueT)
  lazy val Exc: CallDesc = Elem(exc = true)
  def apply(ret: ValueTy): CallDesc = Elem(ret = ret)
}

enum ConstructDesc extends TyElem {
  case Top
  case Elem(exc: Boolean = false, ret: ValueTy = BotT)
  def isBottom: Boolean = this match
    case Top            => false
    case Elem(exc, ret) => !exc && ret.isBottom
  def exists: Boolean = this != Top
  def <=(that: ConstructDesc): Boolean = (this, that) match
    case (_, Top)                     => true
    case (Top, _)                     => false
    case (Elem(le, lr), Elem(re, rr)) => (le <= re) && (lr <= rr)
  def ||(that: ConstructDesc): ConstructDesc = (this, that) match
    case (Top, _) | (_, Top)          => Top
    case (Elem(le, lr), Elem(re, rr)) => Elem(le || re, lr || rr)
  def &&(that: ConstructDesc): ConstructDesc = (this, that) match
    case (_, Top)                     => this
    case (Top, _)                     => that
    case (Elem(le, lr), Elem(re, rr)) => Elem(le && re, lr && rr)
  def getTy: ValueTy = this match
    case Top            => NormalT(ObjectT) || ThrowT
    case Elem(exc, ret) => NormalT(ret) || (if (exc) ThrowT else BotT)
}
object ConstructDesc {
  lazy val Bot: ConstructDesc = Elem()
  lazy val Exist: ConstructDesc = Elem(exc = true, ret = ObjectT)
  lazy val Exc: ConstructDesc = Elem(exc = true)
  def apply(ret: ValueTy): ConstructDesc = Elem(ret = ret)
}

given Ordering[Property] = Ordering.by {
  case Property.PStr(str) => (0, str)
  case Property.PSym(sym) => (1, sym)
}
