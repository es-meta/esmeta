package esmeta.analyzer.domain.heap

import esmeta.LINE_SEP
import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.cfg.*
import esmeta.es
import esmeta.ir.*
import esmeta.state.*
import esmeta.util.*
import esmeta.util.Appender
import esmeta.util.Appender.{*, given}
import esmeta.util.BaseUtils.*

trait HeapBasicDomainDecl { self: Self =>

  /** basic domain for heaps */
  object HeapBasicDomain extends HeapDomain {

    /** elements */
    case class Elem(map: Map[Part, AbsObj], merged: Set[Part])
      extends Appendable

    /** top element */
    lazy val Top: Elem = exploded("top abstract heap")

    /** bottom element */
    val Bot: Elem = Elem(Map(), Set())

    /** abstraction functions */
    def alpha(xs: Iterable[Heap]): Elem = Top

    /** constructors */
    def apply(
      map: Map[Part, AbsObj] = Map(),
      merged: Set[Part] = Set(),
    ): Elem = Elem(map, merged)

    /** extractors */
    def unapply(elem: Elem): (Map[Part, AbsObj], Set[Part]) =
      (elem.map, elem.merged)

    /** appender */
    given rule: Rule[Elem] = mkRule(true)

    /** simpler appender */
    val shortRule: Rule[Elem] = mkRule(false)

    /** set bases */
    def setBase(heap: Heap): Unit = base = (for {
      (addr, obj) <- heap.map
      part = Part.from(addr)
      aobj = AbsObj(obj)
    } yield part -> aobj).toMap
    private var base: Map[Part, AbsObj] = Map()

    /** element interfaces */
    extension (elem: Elem) {

      /** bottom check */
      override def isBottom = elem.map.isEmpty

      /** partial order */
      def ⊑(that: Elem): Boolean = (elem, that) match
        case _ if elem.isBottom => true
        case _ if that.isBottom => false
        case (l, r) => (
          (l.map.keySet ++ r.map.keySet).forall(part => {
            elem(part) ⊑ that(part)
          }) && (l.merged subsetOf r.merged)
        )

      /** join operator */
      def ⊔(that: Elem): Elem = (elem, that) match
        case _ if elem.isBottom => that
        case _ if that.isBottom => elem
        case (l, r) =>
          Elem(
            map = (l.map.keySet ++ r.map.keySet).toList
              .map(part => {
                part -> elem(part) ⊔ that(part)
              })
              .toMap,
            merged = l.merged ++ r.merged,
          )

      /** join operator */
      override def ⊓(that: Elem): Elem = (elem, that) match
        case _ if elem.isBottom || that.isBottom => Bot
        case (Elem(lmap, lmerged), relem @ Elem(rmap, rmerged)) =>
          val newMap = for {
            (part, lobj) <- lmap
            newObj = lobj ⊓ relem(part) if !newObj.isBottom
          } yield part -> newObj
          if (newMap.isEmpty) Bot
          else
            Elem(
              map = newMap.toMap,
              merged = lmerged intersect rmerged,
            )

      /** singleton checks */
      def isSingle: Boolean =
        elem.map.forall { case (part, obj) => isSingle(part) && obj.isSingle }

      /** singleton address partition checks */
      def isSingle(apart: AbsPart): Boolean = apart.getSingle match
        case One(part) => isSingle(part)
        case _         => false
      def isSingle(part: Part): Boolean = !(elem.merged contains part)

      /** handle calls */
      def doCall: Elem = elem
      def doProcStart(fixed: Set[Part]): Elem = elem

      /** handle returns (elem: caller heaps / retHeap: return heaps) */
      def doReturn(to: Elem): Elem = elem
      def doProcEnd(to: Elem): Elem = elem

      /** get reachable address partitions */
      def reachableParts(initParts: Set[Part]): Set[Part] =
        var visited = Set[Part]()
        var reached = Set[Part]()
        def aux(part: Part): Unit = if (!visited.contains(part)) {
          visited += part
          if (!part.isNamed) reached += part
          elem(part).reachableParts.filter(!_.isNamed).foreach(aux)
        }
        elem.map.keys.filter(_.isNamed).foreach(aux)
        initParts.filter(!_.isNamed).foreach(aux)
        reached

      /** remove given address partitions */
      def removeParts(parts: Part*): Elem = elem
      def removeParts(parts: Set[Part]): Elem = elem

      /** lookup abstract address partitions */
      def apply(part: Part): AbsObj =
        elem.map.getOrElse(part, base.getOrElse(part, AbsObj.Bot))
      def apply(part: AbsPart, field: AbsValue): AbsValue =
        part.map(elem(_, field)).foldLeft(AbsValue.Bot: AbsValue)(_ ⊔ _)
      def apply(part: Part, field: AbsValue): AbsValue = part match
        case Named(es.builtin.INTRINSICS) =>
          field.getSingle match
            case Zero => AbsValue.Bot
            case One(str: SimpleValue) =>
              AbsValue(Heap.getIntrinsics(str))
            case One(_) => AbsValue.Bot
            case Many =>
              AbsValue.Top
        case _ => elem(part).get(field)

      /** setters */
      def update(part: AbsPart, field: AbsValue, value: AbsValue): Elem =
        applyEach(elem, part)(_.update(field, value, _))

      /** delete */
      def delete(part: AbsPart, field: AbsValue): Elem =
        applyEach(elem, part)(_.delete(field, _))

      /** concat */
      def concat(part: AbsPart, value: AbsValue): Elem =
        val obj = value.part.foldLeft[AbsObj](AbsObj.Bot) {
          case (obj, part) => obj ⊔ apply(part)
        }
        applyEach(elem, part)(_.concat(obj, _))

      /** append */
      def append(part: AbsPart, value: AbsValue): Elem =
        applyEach(elem, part)(_.append(value, _))

      /** prepend */
      def prepend(part: AbsPart, value: AbsValue): Elem =
        applyEach(elem, part)(_.prepend(value, _))

      /** pops */
      def pop(part: AbsPart, front: Boolean): (AbsValue, Elem) =
        var v: AbsValue = AbsValue.Bot
        val h: Elem = applyEach(elem, part)((obj, weak) => {
          val (newV, newObj) = obj.pop(weak, front)
          v ⊔= newV
          newObj
        })
        (v, h)

      /** remove */
      def remove(part: AbsPart, value: AbsValue): Elem =
        applyEach(elem, part)(_.remove(value, _))

      /** copy objects */
      def copyObj(from: AbsPart)(to: AllocSite): Elem =
        alloc(elem, to, applyFold(elem, from)(obj => obj))

      /** keys of map */
      def keys(part: AbsPart, intSorted: Boolean)(to: AllocSite): Elem =
        alloc(elem, to, applyFold(elem, part)(_.keys(intSorted)))

      /** has SubMap */
      def hasSubMap(tname: String): Boolean =
        (tname endsWith "Object") || (tname endsWith "EnvironmentRecord")

      /** allocation of map with address partitions */
      def allocMap(
        to: AllocSite,
        tname: String,
        pairs: Iterable[(AbsValue, AbsValue)],
      ): Elem =
        given CFG = cfg
        // TODO : Check if this line is okay
        val newObj = pairs.foldLeft(AbsObj(MapObj())) {
          case (m, (k, v)) => m.update(k, v, weak = false)
        }
        alloc(elem, to, newObj)

      /** allocation of record with address partitions */
      def allocRecord(
        to: AllocSite,
        tname: String,
        // TODO : AbsValue -> AbsStr
        pairs: Iterable[(AbsValue, AbsValue)],
      ): Elem =
        given CFG = cfg
        val newObj = pairs.foldLeft(AbsObj(RecordObj(tname))) {
          case (m, (k, v)) => m.update(k, v, weak = false)
        }
        if (hasSubMap(tname)) {
          val subMapPart = SubMap(to)
          val subMapObj = AbsObj(MapObj())
          val newElem = alloc(
            elem,
            to,
            newObj.update(
              AbsValue("SubMap"),
              AbsValue(subMapPart),
              weak = false,
            ),
          )
          alloc(newElem, subMapPart, subMapObj)
        } else alloc(elem, to, newObj)

      /** allocation of list with address partitions */
      def allocList(
        to: AllocSite,
        values: Iterable[AbsValue],
      ): Elem = alloc(elem, to, AbsObj.getList(values))

      /** allocation of symbol with address partitions */
      def allocSymbol(to: AllocSite, desc: AbsValue): Elem =
        alloc(elem, to, AbsObj.getSymbol(desc))

      /** set type of objects */
      def setType(part: AbsPart, tname: String): Elem =
        applyEach(elem, part)((obj, _) => obj.setType(tname))

      /** check contains */
      def contains(part: AbsPart, value: AbsValue): AbsValue =
        part.toList.foldLeft(AbsValue.Bot: AbsValue) {
          case (bool, part) => bool ⊔ (elem(part) contains value)
        }

      /** conversion to string */
      def toString(detail: Boolean): String =
        val app = Appender()
        given heapRule: Rule[Elem] = if (detail) rule else shortRule
        app >> elem
        app.toString
    }

    // -------------------------------------------------------------------------
    // private helpers
    // -------------------------------------------------------------------------
    // appender generator
    private def mkRule(detail: Boolean): Rule[Elem] = (app, elem) =>
      val Elem(map, merged) = elem
      if (elem.isBottom) app >> "{}"
      else if (!detail) app >> "{ ... }"
      else
        app.wrap {
          map.toList
            .sortBy(_._1.toString)
            .foreach {
              case (k, v) =>
                app :> "["
                app >> (if (merged contains k) "M" else " ")
                app >> "] " >> s"$k -> " >> v >> LINE_SEP
            }
        }

    // helper for abstract address partitions
    private def applyEach(elem: Elem, part: AbsPart)(
      f: (AbsObj, Boolean) => AbsObj,
    ): Elem =
      val weak = !elem.isSingle(part)
      part.toList.foldLeft(elem) {
        case (heap, part) =>
          val obj = heap(part)
          val newObj = f(obj, weak)
          Elem(
            map = heap.map + (part -> newObj),
            elem.merged,
          )
      }

    // helper for abstract address partitions
    private def applyFold(elem: Elem, part: AbsPart)(
      f: AbsObj => AbsObj,
    ): AbsObj = part.toList.foldLeft(AbsObj.Bot: AbsObj) {
      case (obj, part) => obj ⊔ f(elem(part))
    }

    // allocation helper
    private def alloc(elem: Elem, part: Part, obj: AbsObj): Elem =
      val cur = elem(part)
      if (cur.isBottom) Elem(elem.map + (part -> obj), elem.merged)
      else Elem(elem.map + (part -> (cur ⊔ obj)), elem.merged + part)
  }
}
