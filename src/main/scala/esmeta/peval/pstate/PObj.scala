package esmeta.peval.pstate

// TODO sort imports
import esmeta.peval.domain.*
import esmeta.state.StateElem
import esmeta.state.Value
import esmeta.state.Math
import esmeta.ir.Expr
import scala.collection.mutable.{LinkedHashMap as LMMap, Map as MMap}
import scala.collection.immutable.Map
import esmeta.cfg.CFG
import esmeta.state.Clo
import esmeta.state.Uninit
import esmeta.state.Str
import esmeta.parser.ESValueParser
import esmeta.state.toStringHelper
import esmeta.ir.EClo

// Objects
sealed trait PObj extends StateElem {

  /** safe getter */
  def get(field: Value): Option[PValue] = (this, field) match
    case (r: PRecordObj, Str(f)) =>
      r.map.get(f).collect {
        case v if (!v.ty.avalue.isBottom) => PValue(v.ty.avalue, v.asValidExpr)
      }
    case (m: PMapObj, key) => m.map.get(key)
    case (l: PListObj, Math(decimal)) if decimal.isValidInt =>
      l.values.lift(decimal.toInt)
    case (y: PYetObj, _) => ??? // throw NotSupported(Feature)(y.msg)
    case _               => None

  /** getter */
  def apply(field: Value): PValue =
    get(field).getOrElse(
      ???, // throw InvalidObjField(this, field)
    )

  /** setter */
  def update(field: Value, value: PValue): Unit = (this, field) match
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
        m.map += field -> PValueExistence.uninit
    case _ => ??? // throw InvalidObjOp(this, s"expand $field")

  /** delete */
  def delete(key: Value): Unit = this match
    case m: PMapObj => m.map -= key
    case _          => ??? // throw InvalidObjOp(this, s"delete $key")

  /** push */
  def push(value: PValue, front: Boolean): Unit = this match
    case l: PListObj =>
      if (front) l.values +:= value
      else l.values :+= value
    case _ => ??? // throw InvalidObjOp(this, "push")

  /** pop */
  def pop(front: Boolean): PValue = this match
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

/** record objects */
case class PRecordObj(
  var tname: String,
  map: MMap[String, PValueExistence],
) extends PObj
object PRecordObj {

  /** apply with type model */
  def apply(
    tname: String,
    fs: Iterable[(String, PValueExistence)],
  )(using CFG): PRecordObj =
    val obj = PRecordObj(tname)
    for { ((k, v), idx) <- fs.zipWithIndex }
      obj.map += k -> v
    obj
  def apply(tname: String)(fs: (String, PValueExistence)*)(using
    CFG,
  ): PRecordObj =
    apply(tname, fs)
  def apply(tname: String)(using cfg: CFG): PRecordObj =
    val obj = PRecordObj(tname, MMap.empty[String, PValueExistence])
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
case class PMapObj(map: LMMap[Value, PValue] = LMMap()) extends PObj
object PMapObj {
  def apply(pairs: Iterable[(Value, PValue)]): PMapObj = PMapObj(
    LMMap.from(pairs),
  )
}

/** list objects */
case class PListObj(var values: Vector[PValue] = Vector()) extends PObj

/** not yet supported objects */
case class PYetObj(tname: String, msg: String) extends PObj
