package esmeta.state

import esmeta.cfg.*
import esmeta.error.*
import esmeta.ir.{Func => IRFunc, *}
import esmeta.parser.ESValueParser
import scala.collection.mutable.{Map => MMap, LinkedHashMap => LMMap}
import esmeta.util.ManualInfo.tyModel

// Objects
sealed trait Obj extends StateElem {

  /** getters */
  def apply(field: Value): Value = (this, field) match
    case (r @ RecordObj(_, map), Str(field)) if (r.isCompletion) =>
      field match
        case "Type" | "Value" | "Target" => map.getOrElse(field, Absent)
        case _                           => throw InvalidObjField(r, Str(field))
      map.getOrElse(field, Absent)

    case (RecordObj(_, map), Str(field)) => map.getOrElse(field, Absent)
    case (MapObj(map), key)              => map.getOrElse(key, Absent)
    case (ListObj(values), Math(decimal)) =>
      val idx = decimal.toInt
      if (0 <= idx && idx < values.length) values(idx)
      else Absent
    case (ListObj(values), Str("length")) => Math(values.length)
    case _                                => throw InvalidObjField(this, field)

  /** copy of object */
  def copied: Obj = this match
    case MapObj(map)           => MapObj(LMMap.from(map))
    case ListObj(values)       => ListObj(Vector.from(values))
    case RecordObj(tname, map) => RecordObj(tname, LMMap.from(map))
    case _                     => this

  /** check abrupt completion */
  def isCompletion: Boolean = this match
    case RecordObj("CompletionRecord", map) => true
    case RecordObj(tname, map) => tyModel.isSubTy(tname, "CompletionRecord")
    case _                     => false

  /** check abrupt completion */
  def isAbruptCompletion: Boolean = this match
    case RecordObj(tname, map) =>
      isCompletion && (map.get("Type") match
        case Some(value) => value != ENUM_NORMAL
        case None        => false
      )
    case _ => false

  /** check abrupt completion */
  def isNormalCompletion: Boolean = this match
    case RecordObj(tname, map) =>
      isCompletion && (!isAbruptCompletion) && (map.getOrElse(
        "Target",
        Absent,
      ) match
        case ENUM_EMPTY => true
        case v          => throw InvalidCompTarget(v)
      )
    case _ => false

}

/** record objects */
case class RecordObj(
  var tname: String,
  val map: MMap[String, Value] = MMap(),
) extends Obj {

  /** updates a value */
  def update(field: PureValue, value: Value): this.type = field match
    case Str(field) => map += field -> value; this
    case _          => throw InvalidObjField(this, field)

  /** keys of map */
  def keys: Vector[Value] = map.keys.toVector.map(Str.apply)
}
object RecordObj {

  /** apply with type model */
  def apply(tname: String)(map: (String, Value)*)(using CFG): RecordObj =
    val obj = RecordObj(tname)
    for { ((k, v), idx) <- map.zipWithIndex }
      obj.map += k -> v
    obj

  def apply(tname: String)(using cfg: CFG): RecordObj =
    // TODO do not explicitly store methods in object but use a type model when
    // accessing methods
    val methods = cfg.tyModel.getMethod(tname)
    val obj = RecordObj(tname, MMap())
    for { ((name, fname), idx) <- methods.zipWithIndex }
      obj.map += name -> Clo(cfg.fnameMap(fname), Map())
    obj
}

object NormalCompObj {
  def apply(value: Value)(using State): RecordObj =
    RecordObj(
      "CompletionRecord",
      MMap(
        "Type" -> ENUM_NORMAL,
        "Value" -> value.toPureValue,
        "Target" -> ENUM_EMPTY,
      ),
    )

  def unapply(obj: Obj): Option[Value] =
    obj match
      case r @ RecordObj(tname, map)
          if (tyModel.isSubTy(tname, "CompletionRecord")) => {
        (r(Str("Type")), r(Str("Target"))) match
          case (ENUM_NORMAL, ENUM_EMPTY) =>
            Some(r(Str("Value")))
          case _ => None
      }
      case _ => None
}

/** map objects */
case class MapObj(
  val map: LMMap[Value, Value] = LMMap(),
) extends Obj {

  /** updates */
  def update(key: Value, value: Value): this.type =
    map += key -> value
    this

  /** deletes */
  def delete(key: Value): this.type = { map -= key; this }

  /** keys of map */
  def keys: Vector[Value] = keys(intSorted = false)
  def keys(intSorted: Boolean): Vector[Value] = if (intSorted) {
    (for {
      case (Str(s), _) <- map.toVector
      d = ESValueParser.str2number(s).double
      if toStringHelper(d) == s
      i = d.toLong // should handle unsigned integer
      if d == i
    } yield (s, i)).sortBy(_._2).map { case (s, _) => Str(s) }
  } else map.keys.toVector
}
object MapObj {
  def apply(pairs: Iterable[(Value, Value)]): MapObj =
    MapObj(LMMap.from(pairs))
}

/** list objects */
case class ListObj(var values: Vector[Value] = Vector()) extends Obj {

  /** updates a value */
  def update(field: Value, value: Value): this.type =
    val idx = field.asInt
    values = values.updated(idx, value)
    this

  /** appends a value */
  def append(value: Value): this.type = { values :+= value; this }

  /** prepends a value */
  def prepend(value: Value): this.type = { values +:= value; this }

  /** pops a value */
  def pop(front: Boolean): Value =
    val vs = values
    if (vs.isEmpty) throw OutOfRange(this, 0)
    val v = if (front) vs.head else vs.last
    values = if (front) vs.drop(1) else vs.dropRight(1)
    v
}

/** not yet supported objects */
case class YetObj(tname: String, msg: String) extends Obj
