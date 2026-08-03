package esmeta.state

import esmeta.cfg.*
import esmeta.error.*
import esmeta.error.NotSupported.Category.*
import esmeta.error.NotSupported.{*, given}
import esmeta.ir.{Func => IRFunc, *}
import esmeta.parser.ESValueParser
import scala.collection.mutable.{Map => MMap, LinkedHashMap => LMMap}

// Objects
sealed trait Obj extends StateElem {

  /** safe getter */
  def get(field: Value): Option[Value] = (this, field) match
    case (r: RecordObj, Str(f)) => r.get(f)
    case (m: MapObj, key)       => m.map.get(key)
    case (l: ListObj, Math(decimal)) if decimal.isValidInt =>
      l.values.lift(decimal.toInt)
    case (y: YetObj, _) => throw NotSupported(Feature)(y.msg)
    case _              => None

  /** getter */
  def apply(field: Value): Value = get(field) match
    case Some(value: Value) => value
    case _                  => throw InvalidObjField(this, field)

  /** setter */
  def update(field: Value, value: Value): Unit = (this, field) match
    case (r: RecordObj, Str(field)) => r.map += field -> value
    case (m: MapObj, key)           => m.map += key -> value
    case (l: ListObj, Math(decimal)) if decimal.isValidInt =>
      val i = decimal.toInt
      val vs = l.values
      if (vs.isDefinedAt(i)) l.values = vs.updated(i, value)
      else throw InvalidObjField(this, field)
    case (y: YetObj, _) => throw NotSupported(Feature)(y.msg)
    case _              => throw InvalidObjField(this, field)

  /** existence check */
  def exists(field: Value): Boolean = (this, field) match
    case (r: RecordObj, Str(field)) => r.map.contains(field)
    case (m: MapObj, key)           => m.map.contains(key)
    case (l: ListObj, Math(decimal)) =>
      0 <= decimal.toInt && decimal.toInt < l.values.length
    case (y: YetObj, _) => throw NotSupported(Feature)(y.msg)
    case _              => false

  /** size */
  def size: Int = this match
    case l: ListObj => l.values.length
    case _          => throw InvalidSizeOf(this)

  /** expand */
  def expand(field: Value): Unit = (this, field) match
    case (m: RecordObj, Str(field)) =>
      if (!m.map.contains(field)) m.map += field -> Undef
    case _ => throw InvalidObjOp(this, s"expand $field")

  /** delete */
  def delete(key: Value): Unit = this match
    case m: MapObj => m.map -= key
    case _         => throw InvalidObjOp(this, s"delete $key")

  /** push */
  def push(value: Value, front: Boolean): Unit = this match
    case l: ListObj =>
      if (front) l.values +:= value
      else l.values :+= value
    case _ => throw InvalidObjOp(this, "push")

  /** pop */
  def pop(front: Boolean): Value = this match
    case l: ListObj if l.values.nonEmpty =>
      val value = if (front) l.values.head else l.values.last
      l.values = if (front) l.values.drop(1) else l.values.dropRight(1)
      value
    case v => throw InvalidObjOp(v, "pop")

  /** copy of object */
  def copied: Obj = this match
    case RecordObj(tname, map) => RecordObj(tname, MMap.from(map))
    case MapObj(map)           => MapObj(LMMap.from(map))
    case ListObj(values)       => ListObj(Vector.from(values))
    case YetObj(tname, msg)    => YetObj(tname, msg)

  /** keys of map */
  def keys(intSorted: Boolean): Vector[Value] = this match
    case r: RecordObj => r.map.keys.toVector.map(Str.apply)
    case m: MapObj if intSorted =>
      (for {
        case (Str(s), _) <- m.map.toVector
        classified = Obj.classifyIntegerKey(s)
        i <- classified.index
      } yield (s, i)).sortBy(_._2).map { case (s, _) => Str(s) }
    case m: MapObj => m.map.keys.toVector
    case _         => throw InvalidObjOp(this, "keys")
}

object Obj {

  /** All host-dependent intermediate values used by integer-sorted map keys.
    *
    * The outer option records whether the `Double.toLong` step was reached
    * after the canonical-string check. The inner option is the exact JVM
    * contract: `Some(d.toLong)` iff Scala's `d == d.toLong`, including its
    * saturation and Double/Long widening behavior at the Long boundaries.
    */
  case class IntegerKeyClassification(
    number: Number,
    rendered: String,
    checkedLong: Option[Option[Long]],
  ) {
    def index: Option[Long] = checkedLong.flatten
  }

  /** Convert a Double using exactly the check previously embedded in
    * `Obj.keys`; do not replace this with range or finiteness predicates.
    */
  def doubleToLongChecked(d: Double): Option[Long] = {
    val i = d.toLong
    Option.when(d == i)(i)
  }

  /** Classify a String key while exposing the exact intermediate host
    * computations to consumers such as the formal-validation exporter.
    */
  def classifyIntegerKey(s: String): IntegerKeyClassification = {
    val number = ESValueParser.str2number(s)
    val rendered = toStringHelper(number.double)
    val checked =
      Option.when(rendered == s)(doubleToLongChecked(number.double))
    IntegerKeyClassification(number, rendered, checked)
  }
}

/** record objects */
case class RecordObj(
  var tname: String,
  map: MMap[String, Value],
) extends Obj {

  /** safe getter */
  def get(field: String): Option[Value] = map.get(field)
}
object RecordObj {

  /** apply with type model */
  def apply(
    tname: String,
    fs: Iterable[(String, Value)],
  )(using CFG): RecordObj =
    val obj = RecordObj(tname)
    for { ((k, v), idx) <- fs.zipWithIndex }
      obj.map += k -> v
    obj
  def apply(tname: String)(fs: (String, Value)*)(using CFG): RecordObj =
    apply(tname, fs)
  def apply(tname: String)(using cfg: CFG): RecordObj =
    new RecordObj(tname, MMap())
}

/** map objects */
case class MapObj(map: LMMap[Value, Value] = LMMap()) extends Obj
object MapObj {
  def apply(pairs: Iterable[(Value, Value)]): MapObj = MapObj(LMMap.from(pairs))
}

/** list objects */
case class ListObj(var values: Vector[Value] = Vector()) extends Obj

/** not yet supported objects */
case class YetObj(tname: String, msg: String) extends Obj
