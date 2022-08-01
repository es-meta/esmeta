package esmeta.analyzer.domain

import esmeta.LINE_SEP
import esmeta.analyzer.*
import esmeta.interp.*
import esmeta.js
import esmeta.util.Appender
import esmeta.util.Appender.*
import esmeta.cfg.CFG

/** basic abstract heaps */
object BasicHeapDomain extends Domain {
  given CFG = cfg

  // base heap
  lazy val baseHeap: Heap = new js.Initialize(cfg).initHeap

  // bottom element
  val Bot = Elem(Map(), Set())

  // base mapping from locations to abstract objects
  lazy val base: Map[Loc, AbsObj] = (for {
    (addr, obj) <- baseHeap.map
    loc = Loc.from(addr)
    aobj = AbsObj(obj)
  } yield loc -> aobj).toMap

  // appender
  def mkRule(detail: Boolean): Rule[Elem] = (app, elem) =>
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
  given rule: Rule[Elem] = mkRule(true)
  val shortRule: Rule[Elem] = mkRule(false)

  // constructors
  def apply(
    map: Map[Loc, AbsObj] = Map(),
    merged: Set[Loc] = Set(),
  ): Elem = Elem(map, merged)

  // extractors
  def unapply(elem: Elem) = Some(
    (
      elem.map,
      elem.merged,
    ),
  )

  // elements
  case class Elem(
    map: Map[Loc, AbsObj],
    merged: Set[Loc],
  ) extends ElemTrait {
    // partial order
    override def isBottom = map.isEmpty

    // partial order
    def ⊑(that: Elem): Boolean = (this, that) match
      case _ if this.isBottom => true
      case _ if that.isBottom => false
      case (l, r) => (
        (l.map.keySet ++ r.map.keySet).forall(loc => {
          this(loc) ⊑ that(loc)
        }) &&
        (l.merged subsetOf r.merged)
      )

    // join operator
    def ⊔(that: Elem): Elem = (this, that) match
      case _ if this.isBottom => that
      case _ if that.isBottom => this
      case (l, r) =>
        Elem(
          map = (l.map.keySet ++ r.map.keySet).toList
            .map(loc => {
              loc -> this(loc) ⊔ that(loc)
            })
            .toMap,
          merged = l.merged ++ r.merged,
        )

    // meet operator
    def ⊓(that: Elem): Elem = (this, that) match
      case _ if this.isBottom || that.isBottom => Bot
      case (Elem(lmap, lmerged), relem @ Elem(rmap, rmerged)) =>
        val newMap = for {
          (loc, lobj) <- lmap
          newObj = lobj ⊓ relem(loc) if !newObj.isBottom
        } yield loc -> newObj
        if (newMap.isEmpty) Bot
        else
          Elem(
            map = newMap.toMap,
            merged = lmerged intersect rmerged,
          )

    // singleton checks
    def isSingle: Boolean = map.forall {
      case (loc, obj) => isSingle(loc) && obj.isSingle
    }

    // singleton location checks
    def isSingle(aloc: AbsLoc): Boolean = aloc.getSingle match
      case FlatElem(loc) => isSingle(loc)
      case _             => false
    def isSingle(loc: Loc): Boolean = !(merged contains loc)

    // handle calls
    def doCall: Elem = this
    def doProcStart(fixed: Set[Loc]): Elem = this

    // handle returns (this: caller heaps / retHeap: return heaps)
    def doReturn(to: Elem): Elem = this
    def doProcEnd(to: Elem): Elem = this

    // get reachable locations
    def reachableLocs(initLocs: Set[Loc]): Set[Loc] =
      var visited = Set[Loc]()
      var reached = Set[Loc]()
      def aux(loc: Loc): Unit = if (!visited.contains(loc)) {
        visited += loc
        if (!loc.isNamed) reached += loc
        this(loc).reachableLocs.filter(!_.isNamed).foreach(aux)
      }
      map.keys.filter(_.isNamed).foreach(aux)
      initLocs.filter(!_.isNamed).foreach(aux)
      reached

    // remove given locations
    def removeLocs(locs: Loc*): Elem = this
    def removeLocs(locs: Set[Loc]): Elem = this

    // lookup abstract locations
    def apply(loc: Loc): AbsObj =
      map.getOrElse(loc, base.getOrElse(loc, AbsObj.Bot))
    def apply(loc: AbsLoc, prop: AbsValue): AbsValue =
      loc.map(this(_, prop)).foldLeft(AbsValue.Bot: AbsValue)(_ ⊔ _)
    def apply(loc: Loc, prop: AbsValue): AbsValue = loc match
      case NamedLoc(js.builtin.INTRINSICS) =>
        prop.getSingle match
          case FlatBot => AbsValue.Bot
          case FlatElem(ASimple(str)) =>
            AbsValue(baseHeap.getIntrinsics(str))
          case FlatElem(_) => AbsValue.Bot
          case FlatTop     => ???
      case _ => this(loc)(prop)

    // setters
    def update(loc: AbsLoc, prop: AbsValue, value: AbsValue): Elem =
      applyEach(loc)(_.update(prop, value, _))

    // delete
    def delete(loc: AbsLoc, prop: AbsValue): Elem =
      applyEach(loc)(_.delete(prop, _))

    // appends
    def append(loc: AbsLoc, value: AbsValue): Elem =
      applyEach(loc)(_.append(value, _))

    // prepends
    def prepend(loc: AbsLoc, value: AbsValue): Elem =
      applyEach(loc)(_.prepend(value, _))

    // pops
    def pop(loc: AbsLoc, front: Boolean): (AbsValue, Elem) =
      var v: AbsValue = AbsValue.Bot
      val h: Elem = applyEach(loc)((obj, weak) => {
        val (newV, newObj) = obj.pop(weak, front)
        v ⊔= newV
        newObj
      })
      (v, h)

    // remove
    def remove(loc: AbsLoc, value: AbsValue): Elem =
      applyEach(loc)(_.remove(value, _))

    // copy objects
    def copyObj(
      from: AbsLoc,
    )(to: AllocSite): Elem = alloc(to, applyFold(from)(obj => obj))

    // keys of map
    def keys(
      loc: AbsLoc,
      intSorted: Boolean,
    )(to: AllocSite): Elem = alloc(to, applyFold(loc)(_.keys(intSorted)))

    // has SubMap
    def hasSubMap(tname: String): Boolean =
      (tname endsWith "Object") || (tname endsWith "EnvironmentRecord")

    // map allocations
    def allocMap(
      tname: String,
      pairs: List[(AbsValue, AbsValue)],
    )(to: AllocSite): Elem =
      val newObj = (pairs.foldLeft(AbsObj(MapObj(tname))) {
        case (m, (k, v)) => m.update(k, v, weak = false)
      })
      if (hasSubMap(tname)) {
        val subMapLoc = SubMapLoc(to)
        val subMapObj = AbsObj.OrderedMap("SubMap", Map(), Vector())
        this
          .alloc(
            to,
            newObj.update(AbsValue("SubMap"), AbsValue(subMapLoc), weak = false),
          )
          .alloc(subMapLoc, subMapObj)
      } else this.alloc(to, newObj)

    // list allocations
    def allocList(
      values: Iterable[AbsValue] = Nil,
    )(to: AllocSite): Elem = alloc(to, AbsObj.KeyWiseList(values.toVector))

    // symbol allocations
    def allocSymbol(
      desc: AbsValue,
    )(to: AllocSite): Elem = alloc(to, AbsObj.SymbolElem(desc))

    // allocation helper
    private def alloc(loc: Loc, obj: AbsObj): Elem = this(loc) match {
      case AbsObj.Bot =>
        Elem(
          map = map + (loc -> obj),
          merged = merged,
        )
      case _ =>
        Elem(
          map = map + (loc -> (this(loc) ⊔ obj)),
          merged = merged + loc,
        )
    }

    // set type of objects
    def setType(loc: AbsLoc, tname: String): Elem =
      applyEach(loc)((obj, _) => obj.setType(tname))

    // check contains
    def contains(loc: AbsLoc, value: AbsValue): AbsValue =
      loc.toList.foldLeft(AbsValue.Bot: AbsValue) {
        case (bool, loc) => bool ⊔ (this(loc) contains value)
      }

    // conversion to string
    def toString(detail: Boolean): String =
      val app = new Appender
      given heapRule: Rule[Elem] =
        if (detail) rule else shortRule
      app >> this
      app.toString

    // helper for abstract locations
    private def applyEach(loc: AbsLoc)(
      f: (AbsObj, Boolean) => AbsObj,
    ): Elem = {
      val weak = !isSingle(loc)
      loc.toList.foldLeft(this) {
        case (heap, loc) =>
          val obj = heap(loc)
          val newObj = f(obj, weak)
          Elem(
            map = heap.map + (loc -> newObj),
            merged,
          )
      }
    }
    private def applyFold(loc: AbsLoc)(f: AbsObj => AbsObj): AbsObj = {
      loc.toList.foldLeft(AbsObj.Bot: AbsObj) {
        case (obj, loc) => obj ⊔ f(this(loc))
      }
    }
  }
}
