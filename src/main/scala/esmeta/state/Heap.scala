package esmeta.state

import esmeta.cfg.*
import esmeta.error.*
import esmeta.error.NotSupported.{*, given}
import esmeta.error.NotSupported.Category.*
import esmeta.ir.{Func => IRFunc, *}
import esmeta.es.builtin.*
import esmeta.util.BaseUtils.*
import scala.collection.mutable.{Map => MMap}

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
  def apply(addr: Addr, key: PureValue): Value = apply(addr) match
    case _ if addr == NamedAddr(INTRINSICS) => Heap.getIntrinsics(key)
    case (s: SymbolObj)                     => s(key)
    case (m: MapObj)                        => m(key)
    case (l: ListObj)                       => l(key)
    case YetObj(_, msg)                     => throw NotSupported(Feature)(msg)

  /** setters */
  def update(addr: Addr, field: PureValue, value: Value): this.type =
    apply(addr) match {
      case (m: MapObj)  => m.update(field, value); this
      case (l: ListObj) => l.update(field, value); this
      case v            => error(s"not a map: $v")
    }

  /** delete */
  def delete(addr: Addr, field: PureValue): this.type = apply(addr) match {
    case (m: MapObj) => m.delete(field); this
    case v           => error(s"not a map: $v")
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

  /** remove given elements from list */
  def remove(addr: Addr, value: PureValue): this.type = apply(addr) match {
    case (l: ListObj) => l.remove(value); this
    case v            => error(s"not a list: $v")
  }

  /** copy objects */
  def copyObj(addr: Addr): Addr = alloc(apply(addr).copied)

  /** keys of map */
  def keys(addr: Addr, intSorted: Boolean): Addr = {
    alloc(ListObj(apply(addr) match {
      case (m: MapObj) => m.keys(intSorted)
      case obj         => error(s"not a map: $obj")
    }))
  }

  /** map allocations */
  def allocMap(
    tname: String,
    m: Map[PureValue, PureValue],
  )(using CFG): Addr = {
    val irMap =
      if (tname == "Record") MapObj(tname, MMap(), 0) else MapObj(tname)
    for ((k, v) <- m) irMap.update(k, v)
    if (hasSubMap(tname))
      val subMap = MapObj("SubMap")
      irMap.update(Str("SubMap"), alloc(subMap))
    if (isObject(tname))
      val privateElems = ListObj()
      irMap.update(Str("PrivateElements"), alloc(privateElems))
    alloc(irMap)
  }

  private def isObject(tname: String): Boolean =
    tname endsWith "Object"
  private def isEnvRec(tname: String): Boolean =
    tname endsWith "EnvironmentRecord"
  private def hasSubMap(tname: String): Boolean =
    isObject(tname) || isEnvRec(tname)

  /** list allocations */
  def allocList(list: List[Value]): Addr = alloc(ListObj(list.toVector))

  /** symbol allocations */
  def allocSymbol(desc: PureValue): Addr = alloc(SymbolObj(desc))

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

  // field value getter
  private def getFieldValue(
    addr: Value,
    fieldName: String,
  ): Value = addr match {
    case addr: Addr =>
      val submap = getAddrValue(addr, "SubMap")
      val field = getAddrValue(submap, fieldName)
      apply(field, Str("Value"))
    case _ => error(s"not an address: $addr")
  }

  /** set type of objects */
  def setType(addr: Addr, tname: String): this.type = apply(addr) match {
    case (irMap: MapObj) =>
      irMap.ty = tname; this
    case _ => error(s"invalid type update: $addr")
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
  def getIntrinsics(key: PureValue): Value =
    val keyStr = key match
      case Str(s) if s.startsWith("%") && s.endsWith("%") =>
        s.substring(1, s.length - 1)
      case v => error(s"invalid intrinsics key1: $key")
    intrAddr(keyStr)
}
