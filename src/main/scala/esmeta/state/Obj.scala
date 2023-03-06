package esmeta.state

import esmeta.cfg.*
import esmeta.error.*
import esmeta.ir.{Func => IRFunc, *}
import esmeta.parser.ESValueParser
import scala.collection.mutable.{Map => MMap}

// Objects
sealed trait Obj extends StateElem {

  /** getters */
  def apply(prop: PureValue): Value = (this, prop) match
    case (SymbolObj(desc), Str("Description")) => desc
    case (MapObj(_, props, _), prop) =>
      props.get(prop).fold[Value](Absent)(_.value)
    case (ListObj(values), Math(decimal)) =>
      val idx = decimal.toInt
      if (0 <= idx && idx < values.length) values(idx)
      else Absent
    case (ListObj(values), Str("length")) => Math(values.length)
    case _                                => throw InvalidObjProp(this, prop)

  /** copy of object */
  def copied: Obj = this match
    case MapObj(tname, props, size) => MapObj(tname, MMap.from(props), size)
    case ListObj(values)            => ListObj(Vector.from(values))
    case _                          => this
}

/** map objects */
case class MapObj(
  var ty: String, // TODO handle type
  val props: MMap[PureValue, MapObj.Prop],
  var size: Int,
) extends Obj {

  /** setters */
  def findOrUpdate(prop: PureValue, value: Value): this.type =
    props.get(prop) match
      case Some(_) => this
      case _       => update(prop, value)

  /** updates */
  def update(prop: PureValue, value: Value): this.type =
    val id = props
      .get(prop)
      .map(_.creationTime)
      .getOrElse({ size += 1; size })
    props += prop -> MapObj.Prop(value, id)
    this

  /** deletes */
  def delete(prop: PureValue): this.type = { props -= prop; this }

  /** pairs of map */
  def pairs: Map[PureValue, Value] = (props.map {
    case (k, (MapObj.Prop(v, _))) => k -> v
  }).toMap

  /** keys of map */
  def keys: Vector[PureValue] = keys(intSorted = false)
  def keys(intSorted: Boolean): Vector[PureValue] = {
    if (!intSorted) {
      if (ty == "SubMap")
        props.toVector
          .sortBy(_._2._2)
          .map(_._1)
      else props.toVector.map(_._1).sortBy(_.toString)
    } else
      (for {
        case (Str(s), _) <- props.toVector
        d = ESValueParser.str2Number(s)
        if toStringHelper(d) == s
        i = d.toLong // should handle unsigned integer
        if d == i
      } yield (s, i)).sortBy(_._2).map { case (s, _) => Str(s) }
  }
}
object MapObj {

  /** property values */
  case class Prop(value: Value, creationTime: Int)

  /** apply with type model */
  def apply(tname: String)(props: (PureValue, Value)*)(using CFG): MapObj =
    val obj: MapObj = MapObj(tname)
    for { ((k, v), idx) <- props.zipWithIndex }
      obj.props += k -> Prop(v, idx + obj.size)
    obj.size += props.size
    obj

  def apply(tname: String)(using cfg: CFG): MapObj =
    // TODO do not explicitly store methods in object but use a type model when
    // accessing methods
    val methods = cfg.tyModel.getMethod(tname)
    val obj = MapObj(tname, MMap(), methods.size)
    for { ((name, fname), idx) <- methods.zipWithIndex }
      obj.props += Str(name) -> Prop(Clo(cfg.fnameMap(fname), Map()), idx)
    obj
}

/** list objects */
case class ListObj(var values: Vector[PureValue] = Vector()) extends Obj {

  /** updates a value */
  def update(prop: PureValue, value: Value): this.type =
    val idx = prop.asInt
    values = values.updated(idx, value.toPureValue)
    this

  /** appends a value */
  def append(value: PureValue): this.type = { values :+= value; this }

  /** prepends a value */
  def prepend(value: PureValue): this.type = { values +:= value; this }

  /** pops a value */
  def pop(front: Boolean): PureValue =
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
