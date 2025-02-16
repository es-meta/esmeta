package esmeta.analyzer.es

import esmeta.state.*
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*
import esmeta.domain.*

/** abstract heaps */
trait AbsHeapDecl { self: ESAnalyzer =>
  case class AbsHeap(
    map: Map[AddrPart, AbsObj] = Map(),
    merged: Set[AddrPart] = Set(),
  ) extends Printable[AbsHeap] {
    import ManualInfo.tyModel

    /** lookup address partitions */
    def apply(part: AddrPart): AbsObj =
      map.getOrElse(part, AbsHeap.base.getOrElse(part, AbsObj.Bot))

    /** lookup address partitions with a key */
    def apply(part: AddrPart, key: AbsValue): AbsValue = apply(part)(key)

    /** setters */
    def update(
      addr: AbsAddr,
      field: AbsValue,
      value: AbsValue,
    ): AbsHeap = update(addr)(_.update(field, value, _))

    /** copy object */
    def copyObj(
      part: AddrPart,
      from: AbsValue,
    ): AbsHeap = ???

    /** get keys of a record/map object as a list */
    def keys(
      part: AddrPart,
      base: AbsValue,
      intSorted: Boolean,
    ): AbsHeap = ???

    /** allocate a record object */
    def allocRecord(
      part: AddrPart,
      tname: String,
      pairs: Iterable[(String, AbsValue)],
    ): AbsHeap = this.checkBottom(pairs.map { (k, v) => v }) {
      alloc(
        part,
        AbsRecord(
          baseTyName = tyModel.baseOf(tname),
          fields = pairs.map { _ -> Binding(_) }.toMap,
        ),
      )
    }

    /** allocate a map object */
    def allocMap(
      pairs: Iterable[(AbsValue, AbsValue)],
      part: AddrPart,
    ): AbsHeap = ???

    /** allocate a list object */
    def allocList(
      part: AddrPart,
      vs: Iterable[AbsValue],
    ): AbsHeap = ???

    // allocation helper
    private def alloc(
      part: AddrPart,
      obj: AbsObj,
    ): AbsHeap = map.get(part) match {
      case None      => AbsHeap(map + (part -> obj), merged)
      case Some(cur) => AbsHeap(map + (part -> (cur ⊔ obj)), merged + part)
    }

    // update each address partition
    private def update(addr: AbsAddr)(
      f: (AbsObj, Boolean) => AbsObj,
    ): AbsHeap = addr.foldLeft(this) { (heap, part) =>
      val weak = merged.contains(part)
      val obj = heap(part)
      copy(map = map + (part -> f(obj, weak)))
    }
  }
  object AbsHeap extends Lattice[AbsHeap] with AbsDomain[Heap, AbsHeap] {

    /** bases */
    private lazy val base: Map[AddrPart, AbsObj] = (for {
      (addr, obj) <- cfg.init.initHeap.map
      part = AddrPart(addr)
      aobj = AbsObj(obj)
    } yield part -> aobj).toMap

    /** top element */
    lazy val Top: AbsHeap = exploded("top abstract heap")

    /** bottom element */
    lazy val Bot: AbsHeap = AbsHeap()

    /** abstraction */
    def alpha(elems: Iterable[Heap]): AbsHeap = ???

    // appender generator
    def mkRule(detail: Boolean): Rule[AbsHeap] = (app, heap) => {
      val AbsHeap(map, merged) = heap
      if (heap.isBottom) app >> "{}"
      else if (!detail) app >> "{ ... }"
      else
        app.wrap {
          map.toList.sortBy(_._1.toString).foreach {
            case (k, v) =>
              app :> (if (merged contains k) "*" else " ") >> k >> " -> " >> v
          }
        }
    }
  }

  given Lattice[AbsHeap] = AbsHeap

  given Lattice.Ops[AbsHeap] with
    extension (heap: AbsHeap) {
      def isTop: Boolean = ???
      def isBottom: Boolean = heap.map.isEmpty
      def ⊑(that: AbsHeap): Boolean = ???
      def ⊔(that: AbsHeap): AbsHeap = ???
      def ⊓(that: AbsHeap): AbsHeap = ???
    }

  given AbsDomain.GenericOps[Heap, AbsHeap] with
    extension (heap: AbsHeap) {
      def contains(value: Heap): Boolean = ???
      def toBSet: BSet[Heap] = ???
      def toFlat: Flat[Heap] = ???
    }

  given Rule[AbsHeap] = AbsHeap.mkRule(true)
}
