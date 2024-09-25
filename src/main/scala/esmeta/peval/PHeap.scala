package esmeta.peval

import esmeta.cfg.*
import esmeta.error.*
import esmeta.error.NotSupported.{*, given}
import esmeta.error.NotSupported.Category.*
import esmeta.es.builtin.*
import esmeta.ir.{Func => IRFunc, *}
import esmeta.state.{Addr, Obj, StateElem}
import esmeta.util.BaseUtils.*
import scala.collection.mutable.{Map => MMap}

// TODO sort imports
import esmeta.state.*

/** IR PHeap for partial Evaluation. similar to state/PHeap.scala */
case class PHeap(
  val map: MMap[Addr, PObj] = MMap(),
) extends StateElem {

  /** getter */
  def apply(addr: Addr): PObj = map.getOrElse(addr, throw UnknownAddr(addr))
  def apply(addr: Addr, field: Value): Predict[Value] = apply(addr)(field)

  /** setter */
  def update(addr: Addr, field: Value, value: Predict[Value]): Unit =
    apply(addr).update(field, value)

  /** existence check */
  def exists(addr: Addr, field: Value): Boolean = apply(addr).exists(field)

  /** expand */
  def expand(addr: Addr, field: Value): Unit = apply(addr).expand(field)

  /** delete */
  def delete(addr: Addr, key: Value): Unit = apply(addr).delete(key)

  /** push */
  def push(addr: Addr, value: Predict[Value], front: Boolean): Unit =
    apply(addr).push(value, front)

  /** pops */
  def pop(addr: Addr, front: Boolean): Predict[Value] = apply(addr).pop(front)

  /** copy */
  def copy(newAddr: Addr, addr: Addr): Unit = alloc(newAddr, apply(addr).copied)

  /** keys */
  def keys(addr: Addr, intSorted: Boolean): Addr = ??? // allocList(
  // ???,
  // apply(addr)
  // .keys(intSorted)
  // .map(_.toPValue),
  // )

  /** record allocations */
  def allocRecord(
    addr: Addr,
    tname: String,
    pairs: Iterable[(String, Predict[Value])] = Nil,
  ): Unit = alloc(addr, PRecordObj(tname, pairs))

  /** map allocations */
  def allocMap(addr: Addr, pairs: Iterable[(Value, Predict[Value])]): Unit =
    alloc(addr, PMapObj(pairs))

  /** list allocations */
  def allocList(addr: Addr, vs: Iterable[Predict[Value]]): Unit =
    alloc(addr, PListObj(vs.toVector))

  // allocation helper
  private def alloc(addr: Addr, obj: PObj): Unit =
    this.map += addr -> obj

  /** copied */
  def copied: PHeap = PHeap(
    map.clone(),
  )

}
