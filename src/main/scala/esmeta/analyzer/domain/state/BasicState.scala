package esmeta.analyzer.domain

import esmeta.LINE_SEP
import esmeta.analyzer.*
import esmeta.cfg.CFG
import esmeta.interp.*
import esmeta.ir.*
import esmeta.js
import esmeta.util.Appender
import esmeta.util.Appender.{*, given}
import esmeta.util.BaseUtils.*
import esmeta.util.StateMonad

/** basic abstract states */
case class BasicState(cfg: CFG) extends Domain { StateD =>
  // basic heap domain
  val AbsHeap = BasicHeapDomain(cfg)
  type AbsHeap = AbsHeap.Elem

  // bottom element
  val Bot = Elem(false, Map(), Map(), AbsHeap.Bot)

  // empty state
  lazy val Empty = Elem(true, Map(), Map(), AbsHeap.Bot)

  // base globals
  lazy val base: Map[Id, AbsValue] = (for {
    (x, v) <- new js.Initialize(cfg, "", None).initGlobal.toList
    av = AbsValue(v)
  } yield x -> av).toMap

  // monad helper
  val monad: StateMonad[Elem] = new StateMonad[Elem]

  // appender
  def mkRule(detail: Boolean): Rule[Elem] = (app, elem) => {
    val irStringifier = IRElem.getStringifier(true, false)
    import irStringifier.given
    given heapRule: Rule[AbsHeap] =
      if (detail) AbsHeap.rule else AbsHeap.shortRule
    if (elem.isBottom) app >> "⊥"
    else {
      app.wrap {
        app :> "locals: " >> elem.locals >> LINE_SEP
        app :> "globals: " >> elem.globals >> LINE_SEP
        app :> "heaps: " >> elem.heap >> LINE_SEP
      }
    }
  }
  given rule: Rule[Elem] = mkRule(true)
  val shortRule: Rule[Elem] = mkRule(false)

  // constructors
  def apply(
    reachable: Boolean = true,
    locals: Map[Id, AbsValue] = Map(),
    globals: Map[Id, AbsValue] = Map(),
    heap: AbsHeap = AbsHeap.Bot,
  ): Elem = Elem(reachable, locals, globals, heap)

  // extractors
  def unapply(elem: Elem) = Some(
    (
      elem.reachable,
      elem.locals,
      elem.globals,
      elem.heap,
    ),
  )

  // elements
  case class Elem(
    reachable: Boolean,
    locals: Map[Id, AbsValue],
    globals: Map[Id, AbsValue],
    heap: AbsHeap,
  ) extends ElemTrait {
    // partial order
    override def isBottom = !this.reachable

    // partial order
    def ⊑(that: Elem): Boolean = (this, that) match {
      case _ if this.isBottom => true
      case _ if that.isBottom => false
      case (
            Elem(_, llocals, lglobals, lheap),
            Elem(_, rlocals, rglobals, rheap),
          ) => {
        val localsB = (llocals.keySet ++ rlocals.keySet).forall(x => {
          this.lookupLocal(x) ⊑ that.lookupLocal(x)
        })
        val globalsB = (lglobals.keySet ++ rglobals.keySet).forall(x => {
          this.lookupGlobal(x) ⊑ that.lookupGlobal(x)
        })
        val heapB = lheap ⊑ rheap
        localsB && globalsB && heapB
      }
    }

    // join operator
    def ⊔(that: Elem): Elem = (this, that) match {
      case _ if this.isBottom => that
      case _ if that.isBottom => this
      case (
            Elem(_, llocals, lglobals, lheap),
            Elem(_, rlocals, rglobals, rheap),
          ) => {
        val newLocals = (for {
          x <- (llocals.keySet ++ rlocals.keySet).toList
          v = this.lookupLocal(x) ⊔ that.lookupLocal(x)
          if !v.isBottom
        } yield x -> v).toMap
        val newGlobals = (for {
          x <- (lglobals.keySet ++ rglobals.keySet).toList
          v = this.lookupGlobal(x) ⊔ that.lookupGlobal(x)
          if !v.isBottom
        } yield x -> v).toMap
        val newHeap = lheap ⊔ rheap
        Elem(true, newLocals, newGlobals, newHeap)
      }
    }

    // singleton checks
    def isSingle: Boolean = (
      reachable &&
        locals.forall(_._2.isSingle) &&
        globals.forall(_._2.isSingle) &&
        heap.isSingle
    )

    // singleton location checks
    def isSingle(loc: Loc): Boolean = heap.isSingle(loc)

    // find merged parts
    def findMerged: Unit = {
      // visited locations
      var visited = Set[Loc]()

      // auxiliary functions
      def auxValue(
        value: AbsValue,
        path: String,
        locPath: String = "",
      ): Unit = {
        if (!value.isBottom && !value.isSingle) {
          if (locPath == "") println(s"$path is merged: $value")
          else println(s"$path ($locPath) is merged: $value")
        }
        for (loc <- value.reachableLocs) auxLoc(loc, path)
      }
      def auxLoc(loc: Loc, path: String): Unit = if (
        (
          !visited.contains(loc) &&
          heap.map.contains(loc)
        )
      ) {
        visited += loc
        heap(loc) match {
          case AbsObj.Bot =>
          case AbsObj.SymbolElem(desc) =>
            auxValue(desc, s"$path.desc", s"$loc.desc")
          case AbsObj.OrderedMap(_, map, _) =>
            for ((p, v) <- map) {
              auxValue(v, s"$path[$p]", s"$loc[$p]")
            }
          case AbsObj.KeyWiseList(values) =>
            for ((v, k) <- values.zipWithIndex) {
              auxValue(v, s"$path[$k]", s"$loc[$k]")
            }
          case AbsObj.NotSupportedElem(_, _) =>
          case obj => println(s"$path ($loc) is merged object: $obj")
        }
      }

      for ((x, v) <- locals) auxValue(v, s"local $x")
      for ((x, v) <- globals) auxValue(v, s"global $x")
      for (loc <- heap.map.keys if loc.isNamed) auxLoc(loc, s"$loc")
      for (loc <- heap.map.keys if !loc.isNamed) auxLoc(loc, s"<unreachable>")
    }

    // handle calls
    def doCall: Elem = this
      .copy(heap = heap.doCall)
      .garbageCollected
    def doProcStart(fixed: Set[Loc]): Elem = this
      .copy(heap = heap.doProcStart(fixed))
      .garbageCollected

    // handle returns (this: return states / to: caller states)
    def doReturn(to: Elem, defs: (Id, AbsValue)*): Elem = doReturn(to, defs)
    def doReturn(to: Elem, defs: Iterable[(Id, AbsValue)]): Elem = Elem(
      reachable = true,
      locals = to.locals ++ defs,
      globals = globals,
      heap = heap.doReturn(to.heap),
    ).garbageCollected
    def doProcEnd(to: Elem, defs: (Id, AbsValue)*): Elem = doProcEnd(to, defs)
    def doProcEnd(to: Elem, defs: Iterable[(Id, AbsValue)]): Elem = Elem(
      reachable = true,
      locals = to.locals ++ defs,
      globals = globals,
      heap = heap.doProcEnd(to.heap),
    ).garbageCollected

    // garbage collection
    def garbageCollected: Elem = this
    // if (USE_GC) {
    //   val unreachLocs = (heap.map.keySet -- reachableLocs).filter(!_.isNamed)
    //   copy(heap = heap.removeLocs(unreachLocs))
    // } else this

    // get reachable locations
    def reachableLocs: Set[Loc] = {
      var locs = Set[Loc]()
      for ((_, v) <- locals) locs ++= v.reachableLocs
      for ((_, v) <- globals) locs ++= v.reachableLocs
      heap.reachableLocs(locs)
    }

    // lookup variable directly
    def directLookup(x: Id): AbsValue = lookupLocal(x) ⊔ lookupGlobal(x)

    // getters
    def apply(rv: AbsRefValue, cp: ControlPoint): AbsValue = rv match {
      case AbsRefId(x)            => this(x, cp)
      case AbsRefProp(base, prop) => this(base, prop)
    }
    def apply(x: Id, cp: ControlPoint): AbsValue = {
      val v = directLookup(x)
      if (cp.isBuiltin && AbsValue.absent ⊑ v) v.removeAbsent ⊔ AbsValue.undef
      else v
    }
    def apply(base: AbsValue, prop: AbsValue): AbsValue = {
      val compValue = base.comp(prop)
      val locValue = heap(base.loc, prop)
      val strValue = (base.str.getSingle, prop.getSingle) match {
        case (FlatBot, _) | (_, FlatBot) => AbsValue.Bot
        case (FlatElem(Str(str)), FlatElem(AMath(k))) =>
          AbsValue(CodeUnit(str(k.toInt)))
        case (FlatElem(Str(str)), FlatElem(ASimple(simple))) =>
          simple match
            case Str("length") => AbsValue(Math(BigDecimal.exact(str.length)))
            case Number(k)     => AbsValue(CodeUnit(str(k.toInt)))
            case _             => AbsValue.Bot
        case _ => AbsValue.str
      }
      compValue ⊔ locValue ⊔ strValue
    }
    def apply(loc: Loc): AbsObj = heap(loc)

    // lookup local variables
    def lookupLocal(x: Id): AbsValue = this match {
      case Elem(_, locals, _, _) =>
        locals.getOrElse(x, AbsValue.Bot)
    }

    // lookup global variables
    def lookupGlobal(x: Id): AbsValue = this match {
      case Elem(_, _, globals, _) =>
        globals.getOrElse(x, base.getOrElse(x, AbsValue.Bot))
    }

    // setters
    def update(refV: AbsRefValue, value: AbsValue): Elem = refV match {
      case AbsRefId(x) => update(x, value)
      case AbsRefProp(base, prop) =>
        update(base.escaped.loc, prop, value)
    }
    def update(x: Id, value: AbsValue): Elem =
      bottomCheck(value) {
        if (locals contains x) copy(locals = locals + (x -> value))
        else copy(globals = globals + (x -> value))
      }
    def update(aloc: AbsLoc, prop: AbsValue, value: AbsValue): Elem =
      bottomCheck(aloc, prop, value) {
        copy(heap = heap.update(aloc, prop, value))
      }

    // existence checks
    def exists(ref: AbsRefValue): AbsBool = ref match {
      case AbsRefId(id)           => directLookup(id).isAbsent
      case AbsRefProp(base, prop) => this(base.escaped, prop).isAbsent
    }

    // delete a property from a map
    def delete(refV: AbsRefValue): Elem = refV match {
      case AbsRefId(x) => error(s"cannot delete variable $x")
      case AbsRefProp(base, prop) =>
        bottomCheck(base, prop) {
          copy(heap = heap.delete(base.escaped.loc, prop))
        }
    }

    // object operators
    def append(loc: AbsLoc, value: AbsValue): Elem =
      bottomCheck(loc, value) { copy(heap = heap.append(loc, value)) }
    def prepend(loc: AbsLoc, value: AbsValue): Elem =
      bottomCheck(loc, value) { copy(heap = heap.prepend(loc, value)) }
    def pop(loc: AbsLoc, idx: AbsValue): (AbsValue, Elem) = {
      var v: AbsValue = AbsValue.Bot
      val st: Elem = bottomCheck(loc, idx) {
        val (newV, newH) = heap.pop(loc, idx)
        v ⊔= newV
        copy(heap = newH)
      }
      (v, st)
    }
    def copyObj(from: AbsLoc)(to: AllocSite): Elem =
      bottomCheck(from) { copy(heap = heap.copyObj(from)(to)) }
    def keys(loc: AbsLoc, intSorted: Boolean)(to: AllocSite): Elem =
      bottomCheck(loc) { copy(heap = heap.keys(loc, intSorted)(to)) }
    def allocMap(tname: String, pairs: List[(AbsValue, AbsValue)])(
      to: AllocSite,
    ): Elem =
      bottomCheck(pairs.flatMap { case (k, v) => List(k, v) }) {
        copy(heap = heap.allocMap(tname, pairs)(to))
      }
    def allocList(list: List[AbsValue])(to: AllocSite): Elem =
      bottomCheck(list) { copy(heap = heap.allocList(list)(to)) }
    def allocSymbol(desc: AbsValue)(to: AllocSite): Elem =
      bottomCheck(desc) { copy(heap = heap.allocSymbol(desc)(to)) }
    def setType(loc: AbsLoc, tname: String): Elem =
      bottomCheck(loc) { copy(heap = heap.setType(loc, tname)) }
    def contains(loc: AbsLoc, value: AbsValue): AbsBool =
      heap.contains(loc, value)

    // define global variables
    def defineGlobal(pairs: (Id, AbsValue)*): Elem =
      bottomCheck(pairs.unzip._2) { copy(globals = globals ++ pairs) }

    // define local variables
    def defineLocal(pairs: (Id, AbsValue)*): Elem =
      bottomCheck(pairs.unzip._2) { copy(locals = locals ++ pairs) }

    // conversion to string
    def toString(detail: Boolean): String = {
      val app = new Appender
      given Rule[Elem] =
        if (detail) StateD.rule else StateD.shortRule
      app >> this
      app.toString
    }

    // get string wth detailed shapes of locations
    def getString(value: AbsValue): String = {
      val app = new Appender
      app >> value.toString
      val locs = value.reachableLocs
      if (!locs.isEmpty) (app >> " @ ").wrap(for (loc <- locs) {
        val obj = heap(loc)
        app :> s"$loc -> " >> obj >> LINE_SEP
      })
      app.toString
    }

    // check bottom elements in abstract semantics
    private def bottomCheck(vs: Domain#ElemTrait*)(f: => Elem): Elem =
      bottomCheck(vs)(f)
    private def bottomCheck(
      vs: Iterable[Domain#ElemTrait],
    )(f: => Elem): Elem = {
      if (this.isBottom || vs.exists(_.isBottom)) Bot
      else f
    }
  }
}
