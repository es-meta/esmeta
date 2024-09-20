package esmeta.peval.pstate

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
import esmeta.peval.domain.*
import esmeta.state.*

/** IR PHeap for partial Evaluation. similar to state/PHeap.scala */
case class PHeap(
  val map: Map[Addr, PObj] = Map(),
  val size: Int = 0,
) extends StateElem {

  /** getter */
  def apply(addr: Addr): PObj = map.getOrElse(addr, throw UnknownAddr(addr))
  def apply(addr: Addr, field: Value): PValue = apply(addr)(field)

  /** setter */
  def update(addr: Addr, field: Value, value: PValue): Unit =
    apply(addr).update(field, value)

  /** existence check */
  def exists(addr: Addr, field: Value): Boolean = apply(addr).exists(field)

  /** expand */
  def expand(addr: Addr, field: Value): Unit = apply(addr).expand(field)

  /** delete */
  def delete(addr: Addr, key: Value): Unit = apply(addr).delete(key)

  /** push */
  def push(addr: Addr, value: PValue, front: Boolean): Unit =
    apply(addr).push(value, front)

  /** pops */
  def pop(addr: Addr, front: Boolean): PValue = apply(addr).pop(front)

  /** copy */
  def copy(addr: Addr): (Addr, PHeap) = alloc(apply(addr).copied)

  /** keys */
  def keys(addr: Addr, intSorted: Boolean): (Addr, PHeap) =
    allocList(
      apply(addr)
        .keys(intSorted)
        .map(_.toPValue),
    )

  /** record allocations */
  def allocRecord(
    tname: String,
    pairs: Iterable[(String, PValue)] = Nil,
  )(using CFG): (Addr, PHeap) = alloc(
    PRecordObj(tname, pairs.map(_ -> PValueExistence.from(_))),
  )

  /** map allocations */
  def allocMap(pairs: Iterable[(Value, PValue)]): (Addr, PHeap) = alloc(
    PMapObj(pairs),
  )

  /** list allocations */
  def allocList(vs: Iterable[PValue]): (Addr, PHeap) = alloc(
    PListObj(vs.toVector),
  )

  // allocation helper
  private def alloc(obj: PObj): (Addr, PHeap) = {
    val newAddr = DynamicAddr(size)
    (
      newAddr,
      PHeap(
        map + (newAddr -> obj),
        size + 1,
      ),
    )
  }

  /** copied */
  def copied: PHeap = this

  def toHeap: esmeta.state.Heap = esmeta.state.Heap(
    ???, // MMap.from(map),
    size,
  )
}

object PHeap {
  def fromHeap(heap: esmeta.state.Heap): PHeap = PHeap(
    ???, // Map.from(heap.map),
    heap.size,
  )
}
