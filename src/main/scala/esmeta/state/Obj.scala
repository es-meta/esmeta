package esmeta.state

import esmeta.cfg.*
import esmeta.error.*
import esmeta.ir.{Func => IRFunc, *}
import esmeta.parser.ESValueParser
import scala.collection.mutable.{Map => MMap}

// Objects
sealed trait Obj extends StateElem {

  /** getters */
  def apply(field: PureValue): Value = (this, field) match
    case (SymbolObj(desc), Str("Description")) => desc
    case (MapObj(_, fields, _), field) =>
      fields.get(field).fold[Value](Absent)(_.value)
    case (ListObj(values), Math(decimal)) =>
      val idx = decimal.toInt
      if (0 <= idx && idx < values.length) values(idx)
      else Absent
    case (ListObj(values), Str("length")) => Math(values.length)
    case _                                => throw InvalidObjField(this, field)

  /** copy of object */
  def copied: Obj = this match
    case MapObj(tname, fields, size) => MapObj(tname, MMap.from(fields), size)
    case ListObj(values)             => ListObj(Vector.from(values))
    case _                           => this
}

/** map objects */
case class MapObj(
  var ty: String, // TODO handle type
  val fields: MMap[PureValue, MapObj.Field],
  var size: Int,
) extends Obj {

  /** setters */
  def findOrUpdate(field: PureValue, value: Value): this.type =
    fields.get(field) match
      case Some(_) => this
      case _       => update(field, value)

  /** updates */
  def update(field: PureValue, value: Value): this.type =
    val id = fields
      .get(field)
      .map(_.creationTime)
      .getOrElse({ size += 1; size })
    fields += field -> MapObj.Field(value, id)
    this

  /** deletes */
  def delete(field: PureValue): this.type = { fields -= field; this }

  /** pairs of map */
  def pairs: Map[PureValue, Value] = (fields.map {
    case (k, (MapObj.Field(v, _))) => k -> v
  }).toMap

  /** keys of map */
  def keys: Vector[PureValue] = keys(intSorted = false)
  def keys(intSorted: Boolean): Vector[PureValue] = {
    if (!intSorted) {
      if (ty == "SubMap")
        fields.toVector
          .sortBy(_._2._2)
          .map(_._1)
      else fields.toVector.map(_._1).sortBy(_.toString)
    } else
      (for {
        case (Str(s), _) <- fields.toVector
        d = ESValueParser.str2number(s).double
        if toStringHelper(d) == s
        i = d.toLong // should handle unsigned integer
        if d == i
      } yield (s, i)).sortBy(_._2).map { case (s, _) => Str(s) }
  }
}
object MapObj {

  /** field values */
  case class Field(value: Value, creationTime: Int)

  /** apply with type model */
  def apply(tname: String)(fields: (PureValue, Value)*)(using CFG): MapObj =
    val obj: MapObj = MapObj(tname)
    for { ((k, v), idx) <- fields.zipWithIndex }
      obj.fields += k -> Field(v, idx + obj.size)
    obj.size += fields.size
    obj

  def apply(tname: String)(using cfg: CFG): MapObj =
    // TODO do not explicitly store methods in object but use a type model when
    // accessing methods
    val methods = cfg.tyModel.getMethod(tname)
    val obj = MapObj(tname, MMap(), methods.size)
    for { ((name, fname), idx) <- methods.zipWithIndex }
      obj.fields += Str(name) -> Field(Clo(cfg.fnameMap(fname), Map()), idx)
    obj
}

/** list objects */
case class ListObj(var values: Vector[Value] = Vector()) extends Obj {

  /** updates a value */
  def update(prop: PureValue, value: Value): this.type =
    val idx = prop.asInt
    values = values.updated(idx, value.toPureValue)
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

  /** remove a value from list */
  def remove(value: PureValue): this.type = {
    values = values.filter(_ != value)
    this
  }
}

/** symbol objects */
case class SymbolObj(desc: PureValue) extends Obj

/** not yet supported objects */
case class YetObj(tname: String, msg: String) extends Obj
