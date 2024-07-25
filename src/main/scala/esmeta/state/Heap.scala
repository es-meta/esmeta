package esmeta.state

import esmeta.cfg.*
import esmeta.error.*
import esmeta.error.NotSupported.{*, given}
import esmeta.error.NotSupported.Category.*
import esmeta.ir.{Func => IRFunc, *}
import esmeta.es.builtin.*
import esmeta.util.BaseUtils.*
import scala.collection.mutable.{Map => MMap, LinkedHashMap => LMMap}

/** IR heaps */
case class Heap(
  val map: MMap[Addr, Obj] = MMap(),
  var size: Int = 0,
) extends StateElem {

  /** getters */
  def apply(addr: Addr): Obj =
    map.getOrElse(addr, throw UnknownAddr(addr)) match
      case YetObj(_, msg) => throw NotSupported(Feature)(msg)
      case obj            => obj
  def apply(addr: Addr, key: Value): Value = apply(addr) match
    case _ if addr == NamedAddr(INTRINSICS) => Heap.getIntrinsics(key)
    case (m: MapObj)                        => m(key)
    case (l: ListObj)                       => l(key)
    case (r: RecordObj)                     => r(key)
    case YetObj(_, msg)                     => throw NotSupported(Feature)(msg)

  /** setters */
  def update(addr: Addr, field: Value, value: Value): this.type =
    apply(addr) match {
      case (m: MapObj)  => m.update(field, value); this
      case (l: ListObj) => l.update(field, value); this
      case (r: RecordObj) => {
        field match
          case prop @ Str(_) => r.update(prop, value)
          case _             => throw InvalidObjField(r, field)
        this
      }
      case v => error(s"not a map: $v")
    }

  /** delete */
  def delete(addr: Addr, field: Value): this.type = apply(addr) match {
    case (m: MapObj)    => m.delete(field); this
    case (r: RecordObj) => error(s"cannot delete from record: $r")
    case v              => error(s"not a map: $v")
  }

  /** appends */
  def append(addr: Addr, value: Value): this.type = apply(addr) match {
    case (l: ListObj) => l.append(value); this
    case v            => error(s"not a list: $v")
  }

  /** prepends */
  def prepend(addr: Addr, value: Value): this.type = apply(addr) match {
    case (l: ListObj) => l.prepend(value); this
    case v            => error(s"not a list: $v")
  }

  /** pops */
  def pop(addr: Addr, front: Boolean): Value = apply(addr) match {
    case (l: ListObj) => l.pop(front)
    case v            => error(s"not a list: $v")
  }

  /** copy objects */
  def copyObj(addr: Addr): Addr = alloc(apply(addr).copied)

  /** keys of map */
  def keys(addr: Addr, intSorted: Boolean): Addr = {
    alloc(ListObj(apply(addr) match {
      case (r: RecordObj) => r.keys
      case (m: MapObj)    => m.keys(intSorted)
      case obj            => error(s"not a map: $obj")
    }))
  }

  /** record allocations */
  def allocRecord(tname: String)(using CFG): Addr =
    val record = RecordObj(tname)
    // TODO check it is the best way to handle this
    if (isObject(tname)) record.update(Str("PrivateElements"), alloc(ListObj()))
    alloc(record)

  private def isObject(tname: String): Boolean =
    tname endsWith "Object"

  /** map allocations */
  def allocMap: Addr = alloc(MapObj())

  /** list allocations */
  def allocList(list: List[Value]): Addr = alloc(ListObj(list.toVector))

  // allocation helper
  private def alloc(obj: Obj): Addr = {
    val newAddr = DynamicAddr(size)
    map += newAddr -> obj
    size += 1
    newAddr
  }

  // field access helper
  private def getAddrValue(
    addr: Addr,
    fieldName: String,
  ): Addr = apply(addr, Str(fieldName)) match {
    case addr: Addr => addr
    case v          => error(s"not an address: $v")
  }

  /** set type of objects */
  def setType(addr: Addr, tname: String): this.type = apply(addr) match {
    case (irMap: RecordObj) => irMap.tname = tname; this
    case _                  => error(s"invalid type update: $addr")
  }

  /** copied */
  def copied: Heap =
    val newMap = MMap.from(map.toList.map {
      case (addr, obj) => addr -> obj.copied
    })
    Heap(newMap, size)
}
object Heap {

  /** special getter for intrinsics */
  def getIntrinsics(key: Value): Value =
    val keyStr = key match
      case Str(s) if s.startsWith("%") && s.endsWith("%") =>
        s.substring(1, s.length - 1)
      case v => error(s"invalid intrinsics key1: $key")
    intrAddr(keyStr)
}
