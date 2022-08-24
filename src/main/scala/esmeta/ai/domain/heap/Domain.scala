package esmeta.ai.domain.heap

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.state.*
import esmeta.util.Appender.*

/** abstract heap domain */
trait Domain extends domain.Domain[Heap] {

  /** constructors */
  def apply(map: Map[Part, AbsObj] = Map(), merged: Set[Part] = Set()): Elem

  /** extractors */
  def unapply(elem: Elem): Option[(Map[Part, AbsObj], Set[Part])]

  /** simpler appender */
  val shortRule: Rule[Elem]

  /** abstract heap interfaces */
  extension (elem: Elem) {

    /** singleton checks */
    def isSingle: Boolean

    /** singleton address partition checks */
    def isSingle(aloc: AbsPart): Boolean
    def isSingle(loc: Part): Boolean

    /** handle calls */
    def doCall: Elem
    def doProcStart(fixed: Set[Part]): Elem

    /** handle returns (this: caller heaps / retHeap: return heaps) */
    def doReturn(to: Elem): Elem
    def doProcEnd(to: Elem): Elem

    /** get reachable address partitions */
    def reachableParts(initParts: Set[Part]): Set[Part]

    /** remove given address partitions */
    def removeParts(locs: Part*): Elem
    def removeParts(locs: Set[Part]): Elem

    /** lookup abstract address partitions */
    def apply(loc: Part): AbsObj
    def apply(loc: AbsPart, prop: AbsValue): AbsValue
    def apply(loc: Part, prop: AbsValue): AbsValue

    /** setters */
    def update(loc: AbsPart, prop: AbsValue, value: AbsValue): Elem

    /** delete */
    def delete(loc: AbsPart, prop: AbsValue): Elem

    /** appends */
    def append(loc: AbsPart, value: AbsValue): Elem

    /** prepends */
    def prepend(loc: AbsPart, value: AbsValue): Elem

    /** pops */
    def pop(loc: AbsPart, front: Boolean): (AbsValue, Elem)

    /** remove */
    def remove(loc: AbsPart, value: AbsValue): Elem

    /** copy objects */
    def copyObj(from: AbsPart)(to: AllocSite): Elem

    /** keys of map */
    def keys(loc: AbsPart, intSorted: Boolean)(to: AllocSite): Elem

    /** has SubMap */
    def hasSubMap(tname: String): Boolean

    /** map aladdress partitions */
    def allocMap(
      tname: String,
      pairs: List[(AbsValue, AbsValue)],
    )(to: AllocSite): Elem

    /** list aladdress partitions */
    def allocList(values: Iterable[AbsValue] = Nil)(to: AllocSite): Elem

    /** symbol aladdress partitions */
    def allocSymbol(desc: AbsValue)(to: AllocSite): Elem

    /** set type of objects */
    def setType(loc: AbsPart, tname: String): Elem

    /** check contains */
    def contains(loc: AbsPart, value: AbsValue): AbsValue

    /** conversion to string */
    def toString(detail: Boolean): String
  }
}
