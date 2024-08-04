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

  /** getter */
  def apply(addr: Addr): Obj = map.getOrElse(addr, throw UnknownAddr(addr))
  def apply(addr: Addr, field: Value): Value = apply(addr)(field)

  /** setter */
  def update(addr: Addr, field: Value, value: Value): Unit =
    apply(addr).update(field, value)

  /** existence check */
  def exists(addr: Addr, field: Value): Boolean = apply(addr).exists(field)

  /** expand */
  def expand(addr: Addr, field: Value): Unit = apply(addr).expand(field)

  /** delete */
  def delete(addr: Addr, key: Value): Unit = apply(addr).delete(key)

  /** push */
  def push(addr: Addr, value: Value, front: Boolean): Unit =
    apply(addr).push(value, front)

  /** pops */
  def pop(addr: Addr, front: Boolean): Value = apply(addr).pop(front)

  /** copy */
  def copy(addr: Addr): Addr = alloc(apply(addr).copied)

  /** keys */
  def keys(addr: Addr, intSorted: Boolean): Addr =
    allocList(apply(addr).keys(intSorted))

  /** record allocations */
  def allocRecord(
    tname: String,
    pairs: Iterable[(String, Value)] = Nil,
  )(using CFG): Addr = alloc(RecordObj(tname, pairs))

  /** map allocations */
  def allocMap(pairs: Iterable[(Value, Value)]): Addr = alloc(MapObj(pairs))

  /** list allocations */
  def allocList(vs: Iterable[Value]): Addr = alloc(ListObj(vs.toVector))

  // allocation helper
  private def alloc(obj: Obj): Addr = {
    val newAddr = DynamicAddr(size)
    map += newAddr -> obj
    size += 1
    newAddr
  }

  /** copied */
  def copied: Heap = Heap(MMap.from(map.toList.map { _ -> _.copied }), size)
}
