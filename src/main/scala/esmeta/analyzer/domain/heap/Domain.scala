package esmeta.analyzer.domain.heap

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*
import esmeta.util.Appender.*

/** abstract heap domain */
trait Domain extends domain.Domain[Heap] {

  /** constructors */
  def apply(map: Map[Part, AbsObj] = Map(), merged: Set[Part] = Set()): Elem

  /** extractors */
  def unapply(elem: Elem): Option[(Map[Part, AbsObj], Set[Part])]

  /** set bases */
  def setBase(heap: Heap): Unit

  /** simpler appender */
  val shortRule: Rule[Elem]

  /** abstract heap interfaces */
  extension (elem: Elem) {

    /** singleton checks */
    def isSingle: Boolean

    /** singleton address partition checks */
    def isSingle(apart: AbsPart): Boolean
    def isSingle(part: Part): Boolean

    /** handle calls */
    def doCall: Elem
    def doProcStart(fixed: Set[Part]): Elem

    /** handle returns (this: caller heaps / retHeap: return heaps) */
    def doReturn(to: Elem): Elem
    def doProcEnd(to: Elem): Elem

    /** get reachable address partitions */
    def reachableParts(initParts: Set[Part]): Set[Part]

    /** remove given address partitions */
    def removeParts(parts: Part*): Elem
    def removeParts(parts: Set[Part]): Elem

    /** lookup abstract address partitions */
    def apply(part: Part): AbsObj
    def apply(part: AbsPart, prop: AbsValue): AbsValue
    def apply(part: Part, prop: AbsValue): AbsValue

    /** setters */
    def update(part: AbsPart, prop: AbsValue, value: AbsValue): Elem

    /** delete */
    def delete(part: AbsPart, prop: AbsValue): Elem

    /** concat */
    def concat(part: AbsPart, value: AbsValue): Elem

    /** appends */
    def append(part: AbsPart, value: AbsValue): Elem

    /** prepends */
    def prepend(part: AbsPart, value: AbsValue): Elem

    /** pops */
    def pop(part: AbsPart, front: Boolean): (AbsValue, Elem)

    /** remove */
    def remove(part: AbsPart, value: AbsValue): Elem

    /** copy objects */
    def copyObj(from: AbsPart)(to: AllocSite): Elem

    /** keys of map */
    def keys(part: AbsPart, intSorted: Boolean)(to: AllocSite): Elem

    /** has SubMap */
    def hasSubMap(tname: String): Boolean

    /** allocation of map with address partitions */
    def allocMap(
      to: AllocSite,
      tname: String,
      pairs: Iterable[(AbsValue, AbsValue)] = Nil,
    ): Elem

    /** allocation of list with address partitions */
    def allocList(
      to: AllocSite,
      values: Iterable[AbsValue] = Nil,
    ): Elem

    /** allocation of symbol with address partitions */
    def allocSymbol(to: AllocSite, desc: AbsValue): Elem

    /** set type of objects */
    def setType(part: AbsPart, tname: String): Elem

    /** check contains */
    def contains(part: AbsPart, value: AbsValue): AbsValue

    /** conversion to string */
    def toString(detail: Boolean): String
  }
}
