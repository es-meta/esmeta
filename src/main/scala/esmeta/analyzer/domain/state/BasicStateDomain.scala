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
object BasicStateDomain extends StateDomain {

  // TODO remove unsafe type casting
  given Conversion[AbsValue, BasicValueDomain.Elem] =
    _.asInstanceOf[BasicValueDomain.Elem]
  given Conversion[BasicValueDomain.Elem, AbsValue] = _.asInstanceOf[AbsValue]

  /** bottom element */
  val Bot: Elem = Elem(false, Map(), Map(), AbsHeap.Bot)

  /** empty element */
  val Empty: Elem = Elem(true, Map(), Map(), AbsHeap.Bot)

  /** base globals */
  lazy val baseGlobals: Map[Id, AbsValue] = (for {
    (x, v) <- new js.Initialize(cfg).initGlobal.toList
  } yield x -> AbsValue(v)).toMap

  /** appender */
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

  /** elements */
  case class Elem(
    reachable: Boolean,
    locals: Map[Local, AbsValue],
    globals: Map[Global, AbsValue],
    heap: AbsHeap,
  ) extends StateElemTrait {

    /** partial order */
    override def ⊑(that: Elem): Boolean =
      val globalsB = (this.globals.keySet ++ that.globals.keySet).forall(x => {
        this.lookupGlobal(x) ⊑ that.lookupGlobal(x)
      })
      super.⊑(that) && globalsB && this.heap ⊑ that.heap

    /** join operator */
    def ⊔(that: Elem): Elem = (this, that) match
      case _ if this.isBottom => that
      case _ if that.isBottom => this
      case (l, r) => {
        val newLocals = (for {
          x <- (l.locals.keySet ++ r.locals.keySet).toList
          v = this.lookupLocal(x) ⊔ that.lookupLocal(x)
        } yield x -> v).toMap
        val newGlobals = (for {
          x <- (l.globals.keySet ++ r.globals.keySet).toList
          v = this.lookupGlobal(x) ⊔ that.lookupGlobal(x)
        } yield x -> v).toMap
        val newHeap = this.heap ⊔ that.heap
        Elem(true, newLocals, newGlobals, newHeap)
      }

    /** meet operator */
    def ⊓(that: Elem): Elem = (this, that) match
      case _ if this.isBottom || that.isBottom => Bot
      case (l, r) =>
        var isBottom = false
        val newLocals = (for {
          x <- (l.locals.keySet ++ r.locals.keySet).toList
          v = this.lookupLocal(x) ⊓ that.lookupLocal(x)
          _ = isBottom |= v.isBottom
        } yield x -> v).toMap
        val newGlobals = (for {
          x <- (l.globals.keySet ++ r.globals.keySet).toList
          v = this.lookupGlobal(x) ⊓ that.lookupGlobal(x)
          _ = isBottom |= v.isBottom
        } yield x -> v).toMap
        val newHeap = this.heap ⊓ that.heap
        if (newHeap.isBottom || isBottom) Bot
        else Elem(true, newLocals, newGlobals, newHeap)

    /** getters * */
    def apply(base: AbsValue, prop: AbsValue): AbsValue =
      val compValue = base.comp(prop)
      val locValue = heap(base.loc, prop)
      val astValue = lookupAst(base.ast, prop)
      val strValue = lookupStr(base.str, prop)
      compValue ⊔ locValue ⊔ astValue ⊔ strValue
    private def lookupAst(ast: AbsAst, prop: AbsValue): AbsValue =
      (ast.getSingle, prop.getSingle) match {
        case (FlatBot, _) | (_, FlatBot) => AbsValue.Bot
        case (FlatElem(AAst(ast)), FlatElem(ASimple(Str("parent")))) =>
          ast.parent.map(AbsValue(_)).getOrElse(AbsValue(Absent))
        case (
              FlatElem(AAst(syn: js.Syntactic)),
              FlatElem(ASimple(Str(propStr))),
            ) =>
          val js.Syntactic(name, _, rhsIdx, children) = syn
          val rhs = cfg.grammar.nameMap(name).rhsList(rhsIdx)
          rhs.getNtIndex(propStr).flatMap(children(_)) match
            case Some(child) => AbsValue(child)
            case _           => AbsValue.Bot
        case (FlatElem(AAst(syn: js.Syntactic)), FlatElem(AMath(n)))
            if n.isValidInt =>
          syn.children(n.toInt).map(AbsValue(_)).getOrElse(AbsValue(Absent))
        case (_: FlatElem[_], _: FlatElem[_]) => AbsValue.Bot
        case _                                => exploded("ast property access")
      }
    private def lookupStr(str: AbsStr, prop: AbsValue): AbsValue =
      (str.getSingle, prop.getSingle) match {
        case (FlatBot, _) | (_, FlatBot) => AbsValue.Bot
        case (FlatElem(Str(str)), FlatElem(AMath(k))) =>
          AbsValue(CodeUnit(str(k.toInt)))
        case (FlatElem(Str(str)), FlatElem(ASimple(simple))) =>
          simple match
            case Str("length") => AbsValue(Math(BigDecimal.exact(str.length)))
            case Number(k)     => AbsValue(CodeUnit(str(k.toInt)))
            case _             => AbsValue.Bot
        case _ => AbsValue.codeunit ⊔ AbsValue.math
      }
    def apply(loc: Loc): AbsObj = heap(loc)
    def lookupGlobal(x: Global): AbsValue =
      this.globals.getOrElse(x, baseGlobals.getOrElse(x, AbsValue.Bot))

    // setters
    def update(x: Id, value: AbsValue): Elem =
      bottomCheck(value) {
        x match
          case x: Global if globals contains x =>
            copy(globals = globals + (x -> value))
          case x: Name if locals contains x =>
            copy(locals = locals + (x -> value))
          case x: Temp =>
            copy(locals = locals + (x -> value))
          case _ => Bot
      }
    def update(aloc: AbsValue, prop: AbsValue, value: AbsValue): Elem =
      bottomCheck(aloc, prop, value) {
        copy(heap = heap.update(aloc.loc, prop, value))
      }

    // delete a property from a map
    def delete(refV: AbsRefValue): Elem = refV match
      case AbsRefId(x) => error(s"cannot delete variable $x")
      case AbsRefProp(base, prop) =>
        bottomCheck(base, prop) {
          copy(heap = heap.delete(base.loc, prop))
        }

    // default value for bottom check
    given bottomValue: AbsValue = AbsValue.Bot

    // object operators
    def append(loc: AbsLoc, value: AbsValue): Elem =
      bottomCheck(loc, value) { copy(heap = heap.append(loc, value)) }
    def prepend(loc: AbsLoc, value: AbsValue): Elem =
      bottomCheck(loc, value) { copy(heap = heap.prepend(loc, value)) }
    def remove(loc: AbsLoc, value: AbsValue): Elem =
      bottomCheck(loc, value) { copy(heap = heap.remove(loc, value)) }
    def pop(loc: AbsLoc, front: Boolean): (AbsValue, Elem) = {
      var v: AbsValue = AbsValue.Bot
      val st: Elem = bottomCheck(loc) {
        val (newV, newH) = heap.pop(loc, front)
        v ⊔= newV
        copy(heap = newH)
      }
      (v, st)
    }
    def copyObj(from: AbsLoc, to: AllocSite): Elem =
      bottomCheck(from) { copy(heap = heap.copyObj(from)(to)) }
    def keys(loc: AbsLoc, intSorted: Boolean, to: AllocSite): Elem =
      bottomCheck(loc) { copy(heap = heap.keys(loc, intSorted)(to)) }
    def allocMap(
      tname: String,
      pairs: List[(AbsValue, AbsValue)],
      to: AllocSite,
    ): (AbsValue, Elem) =
      val locV = AbsValue(to)
      bottomCheck(pairs.flatMap { case (k, v) => List(k, v) }) {
        (locV, copy(heap = heap.allocMap(tname, pairs)(to)))
      }
    def allocList(list: List[AbsValue], to: AllocSite): (AbsValue, Elem) =
      val locV = AbsValue(to)
      bottomCheck(list) { (locV, copy(heap = heap.allocList(list)(to))) }
    def allocSymbol(desc: AbsValue, to: AllocSite): (AbsValue, Elem) =
      val locV = AbsValue(to)
      val descV = BasicValueDomain(str = desc.str, undef = desc.undef)
      bottomCheck(descV) {
        (locV, copy(heap = heap.allocSymbol(descV)(to)))
      }
    def setType(loc: AbsLoc, tname: String): Elem =
      bottomCheck(loc) { copy(heap = heap.setType(loc, tname)) }
    def contains(
      list: AbsValue,
      value: AbsValue,
      field: Option[(Type, String)],
    ): AbsValue = field match
      case Some(_) => ??? // TODO
      case None    => heap.contains(list.loc, value)

    // define global variables
    def defineGlobal(pairs: (Global, AbsValue)*): Elem =
      bottomCheck(pairs.unzip._2) { copy(globals = globals ++ pairs) }

    // define local variables
    def defineLocal(pairs: (Local, AbsValue)*): Elem =
      bottomCheck(pairs.unzip._2) { copy(locals = locals ++ pairs) }

    // singleton checks
    override def isSingle: Boolean =
      super.isSingle && globals.forall(_._2.isSingle) && heap.isSingle

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
    def doReturn(to: Elem, defs: Iterable[(Local, AbsValue)]): Elem = Elem(
      reachable = true,
      locals = to.locals ++ defs,
      globals = globals,
      heap = heap.doReturn(to.heap),
    ).garbageCollected
    def doProcEnd(to: Elem, defs: (Local, AbsValue)*): Elem =
      doProcEnd(to, defs)
    def doProcEnd(to: Elem, defs: Iterable[(Local, AbsValue)]): Elem = Elem(
      reachable = true,
      locals = to.locals ++ defs,
      globals = globals,
      heap = heap.doProcEnd(to.heap),
    ).garbageCollected

    // TODO garbage collection
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

    // copy
    def copied(
      locals: Map[Local, AbsValue] = Map(),
    ): Elem = copy(locals = locals)

    // conversion to string
    def toString(detail: Boolean): String = {
      val app = new Appender
      given Rule[Elem] =
        if (detail) BasicStateDomain.rule else BasicStateDomain.shortRule
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
  }
}
