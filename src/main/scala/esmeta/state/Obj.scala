package esmeta.state

import esmeta.cfg.*
import esmeta.error.*
import esmeta.ir.{Func => IRFunc, *}
import esmeta.parser.ESValueParser
import scala.collection.mutable.{Map => MMap, LinkedHashMap => LMMap}

// Objects
sealed trait Obj extends StateElem {

  /** getters */
  def apply(field: Value): Value = (this, field) match
    case (SymbolObj(desc), Str("Description")) => desc
    case (MapObj(map), field)                  => map.getOrElse(field, Absent)
    case (ListObj(values), Math(decimal)) =>
      val idx = decimal.toInt
      if (0 <= idx && idx < values.length) values(idx)
      else Absent
    case (ListObj(values), Str("length")) => Math(values.length)
    case (RecordObj(_, map), Str(field)) =>
      map.getOrElse(field, Absent)
    case _ => throw InvalidObjField(this, field)

  /** copy of object */
  def copied: Obj = this match
    case MapObj(map)           => MapObj(LMMap.from(map))
    case ListObj(values)       => ListObj(Vector.from(values))
    case RecordObj(tname, map) => RecordObj(tname, LMMap.from(map))
    case _                     => this
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

/** symbol objects */
case class SymbolObj(desc: Value) extends Obj

/** not yet supported objects */
case class YetObj(tname: String, msg: String) extends Obj
