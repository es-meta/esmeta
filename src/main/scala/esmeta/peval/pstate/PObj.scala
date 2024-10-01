package esmeta.peval.pstate

// TODO sort imports
import esmeta.ir.{Expr, EClo}
import scala.collection.mutable.{LinkedHashMap as LMMap, Map as MMap}
import scala.collection.immutable.Map
import esmeta.cfg.CFG
import esmeta.state.{
  StateElem,
  Value,
  Math,
  Clo,
  Str,
  Uninit,
  Obj,
  toStringHelper,
}
import esmeta.parser.ESValueParser
import esmeta.state.RecordObj
import esmeta.state.MapObj
import esmeta.state.ListObj
import esmeta.state.YetObj

import esmeta.peval.*

// Objects
sealed trait PObj extends StateElem {

  /** safe getter */
  def get(field: Value): Option[Predict[Value | Uninit]] = (this, field) match
    case (r: PRecordObj, Str(f)) => r.map.get(f)
    case (m: PMapObj, key)       => m.map.get(key)
    case (l: PListObj, Math(decimal)) if decimal.isValidInt =>
      l.values.lift(decimal.toInt)
    case (y: PYetObj, _) => ??? // throw NotSupported(Feature)(y.msg)
    case _               => None

  /** getter */
  def apply(field: Value): Predict[Value] = get(field) match
    case Some(Known(value: Value)) => Known(value)
    case Some(Known(Uninit))       => ??? // throw
    case _                         => ??? // throw InvalidObjField(this, field)

  /** setter */
  def update(field: Value, value: Predict[Value]): Unit = (this, field) match
    case (r: PRecordObj, Str(field)) => // TODO if r.map.contains(field) =>
      ??? // r.map += field -> value
    case (m: PMapObj, key) => m.map += key -> value
    case (l: PListObj, Math(decimal)) if decimal.isValidInt =>
      val i = decimal.toInt
      val vs = l.values
      if (vs.isDefinedAt(i)) l.values = vs.updated(i, value)
      else ??? // throw InvalidObjField(this, field)
    case (y: PYetObj, _) => ??? // throw NotSupported(Feature)(y.msg)
    case _               => ??? // throw InvalidObjField(this, field)

  /** existence check */
  def exists(field: Value): Boolean = (this, field) match
    case (r: PRecordObj, Str(field)) => r.map.contains(field)
    case (m: PMapObj, key)           => m.map.contains(key)
    case (l: PListObj, Math(decimal)) =>
      0 <= decimal.toInt && decimal.toInt < l.values.length
    case (y: PYetObj, _) => ??? // throw NotSupported(Feature)(y.msg)
    case _               => false

  /** size */
  def size: Int = this match
    case l: PListObj => l.values.length
    case _           => ??? // throw InvalidSizeOf(this)

  /** expand */
  def expand(field: Value): Unit = (this, field) match
    case (m: PRecordObj, Str(field)) =>
      if (!m.map.contains(field))
        m.map += field -> Known(Uninit)
    case _ => ??? // throw InvalidObjOp(this, s"expand $field")

  /** delete */
  def delete(key: Value): Unit = this match
    case m: PMapObj => m.map -= key
    case _          => ??? // throw InvalidObjOp(this, s"delete $key")

  /** push */
  def push(value: Predict[Value], front: Boolean): Unit = this match
    case l: PListObj =>
      if (front) l.values +:= value
      else l.values :+= value
    case _ => ??? // throw InvalidObjOp(this, "push")

  /** pop */
  def pop(front: Boolean): Predict[Value] = this match
    case l: PListObj if l.values.nonEmpty =>
      val value = if (front) l.values.head else l.values.last
      l.values = if (front) l.values.drop(1) else l.values.dropRight(1)
      value
    case v => ??? // throw InvalidObjOp(v, "pop")

  /** copy of object */
  def copied: PObj = this match
    case PRecordObj(tname, map) => PRecordObj(tname, MMap.from(map))
    case PMapObj(map)           => PMapObj(LMMap.from(map))
    case PListObj(values)       => PListObj(Vector.from(values))
    case PYetObj(tname, msg)    => PYetObj(tname, msg)
    case PUnknownObj()          => ???

  /** keys of map */
  def keys(intSorted: Boolean): Vector[Value] = this match
    case r: PRecordObj => r.map.keys.toVector.map(Str.apply)
    case m: PMapObj if intSorted =>
      (for {
        case (Str(s), _) <- m.map.toVector
        d = ESValueParser.str2number(s).double
        if toStringHelper(d) == s
        i = d.toLong // should handle unsigned integer
        if d == i
      } yield (s, i)).sortBy(_._2).map { case (s, _) => Str(s) }
    case m: PMapObj => m.map.keys.toVector
    case _          => ??? // throw InvalidObjOp(this, "keys")
}

object PObj {
  def from(obj: Obj): PObj = obj match
    case RecordObj(tname, map) =>
      PRecordObj(tname, map.map((k, v) => (k, Known(v))))
    case MapObj(map)        => PMapObj(map.map((k, v) => (k, Known(v))))
    case ListObj(values)    => PListObj(values.map(Known.apply))
    case YetObj(tname, msg) => PYetObj(tname, msg)
}

/** record objects */
case class PRecordObj(
  var tname: String,
  map: MMap[String, Predict[Value | Uninit]],
) extends PObj
object PRecordObj {

  /** apply with type model */
  def apply(
    tname: String,
    fs: Iterable[(String, Predict[Value | Uninit])],
  ): PRecordObj =
    val obj = PRecordObj(tname)
    for { ((k, v), idx) <- fs.zipWithIndex }
      obj.map += k -> v
    obj
  def apply(tname: String)(fs: (String, Predict[Value | Uninit])*): PRecordObj =
    apply(tname, fs)
  def apply(tname: String): PRecordObj =
    val obj = PRecordObj(tname, MMap.empty[String, Predict[Value | Uninit]])
    // TODO : type model..
    // TODO : "EClo" is not okay for this case..

    // for ((name, fname) <- cfg.tyModel.getMethod(tname))
    //   obj.map += name -> EClo(
    //     cfg.fnameMap(fname).name,
    //     Nil,
    //   ) // Clo(cfg.fnameMap(fname), Map())
    obj
}

/** map objects */
case class PMapObj(map: LMMap[Value, Predict[Value]] = LMMap()) extends PObj
object PMapObj {
  def apply(pairs: Iterable[(Value, Predict[Value])]): PMapObj = PMapObj(
    LMMap.from(pairs),
  )
}

/** list objects */
case class PListObj(var values: Vector[Predict[Value]] = Vector()) extends PObj

/** not yet supported objects */
case class PYetObj(tname: String, msg: String) extends PObj

case class PUnknownObj() extends PObj
