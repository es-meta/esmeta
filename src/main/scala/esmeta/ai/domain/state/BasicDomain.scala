package esmeta.ai.domain.state

// import esmeta.LINE_SEP
import esmeta.ai.*
import esmeta.ai.domain.*
// import esmeta.cfg.CFG
import esmeta.state.*
import esmeta.ir.*
// import esmeta.es
// import esmeta.es.*
import esmeta.util.Appender.{*, given}
// import esmeta.util.BaseUtils.*
// import esmeta.util.StateMonad

/** TODO basic domain for states */
object BasicDomain extends state.Domain {
  // // TODO remove unsafe type casting
  // given Conversion[AbsValue, BasicValueDomain.Elem] =
  //   _.asInstanceOf[BasicValueDomain.Elem]
  // given Conversion[BasicValueDomain.Elem, AbsValue] = _.asInstanceOf[AbsValue]

  /** top element */
  lazy val Top: Elem = exploded("top abstract state")

  /** bottom element */
  val Bot: Elem = Elem(false, Map(), Map()) // TODO , AbsHeap.Bot)

  /** empty element */
  val Empty: Elem = Elem(true, Map(), Map()) // TODO , AbsHeap.Bot)

  /** base globals */
  lazy val baseGlobals: Map[Id, AbsValue] = ??? // (for {
  //   (x, v) <- new es.Initialize(cfg).initGlobal.toList
  // } yield x -> AbsValue(v)).toMap

  /** abstraction functions */
  def alpha(xs: Iterable[State]): Elem = ???

  // /** appender */
  // def mkRule(detail: Boolean): Rule[Elem] = (app, elem) => {
  //   val irStringifier = IRElem.getStringifier(true, false)
  //   import irStringifier.given
  //   given heapRule: Rule[AbsHeap] =
  //     if (detail) AbsHeap.rule else AbsHeap.shortRule
  //   if (elem.isBottom) app >> "⊥"
  //   else {
  //     app.wrap {
  //       app :> "locals: " >> elem.locals >> LINE_SEP
  //       app :> "globals: " >> elem.globals >> LINE_SEP
  //       app :> "heaps: " >> elem.heap >> LINE_SEP
  //     }
  //   }
  // }
  given rule: Rule[Elem] = ??? // mkRule(true)
  // val shortRule: Rule[Elem] = mkRule(false)

  /** TODO elements */
  case class Elem(
    reachable: Boolean,
    locals: Map[Local, AbsValue],
    globals: Map[Global, AbsValue],
    // heap: AbsHeap,
  )

  /** element interfaces */
  extension (elem: Elem) {

    // /** bottom check */
    // def isBottom = !this.reachable

    /** partial order */
    def ⊑(that: Elem): Boolean = ???
    // val globalsB = (elem.globals.keySet ++ that.globals.keySet).forall(x => {
    //   elem.lookupGlobal(x) ⊑ that.lookupGlobal(x)
    // })
    // super.⊑(that) && globalsB && elem.heap ⊑ that.heap

    /** join operator */
    def ⊔(that: Elem): Elem = ??? // (elem, that) match
    //   case _ if elem.isBottom => that
    //   case _ if that.isBottom => elem
    //   case (l, r) => {
    //     val newLocals = (for {
    //       x <- (l.locals.keySet ++ r.locals.keySet).toList
    //       v = elem.lookupLocal(x) ⊔ that.lookupLocal(x)
    //     } yield x -> v).toMap
    //     val newGlobals = (for {
    //       x <- (l.globals.keySet ++ r.globals.keySet).toList
    //       v = elem.lookupGlobal(x) ⊔ that.lookupGlobal(x)
    //     } yield x -> v).toMap
    //     val newHeap = elem.heap ⊔ that.heap
    //     Elem(true, newLocals, newGlobals, newHeap)
    //   }

    /** meet operator */
    override def ⊓(that: Elem): Elem = ??? // (elem, that) match
    //   case _ if elem.isBottom || that.isBottom => Bot
    //   case (l, r) =>
    //     var isBottom = false
    //     val newLocals = (for {
    //       x <- (l.locals.keySet ++ r.locals.keySet).toList
    //       v = elem.lookupLocal(x) ⊓ that.lookupLocal(x)
    //       _ = isBottom |= v.isBottom
    //     } yield x -> v).toMap
    //     val newGlobals = (for {
    //       x <- (l.globals.keySet ++ r.globals.keySet).toList
    //       v = elem.lookupGlobal(x) ⊓ that.lookupGlobal(x)
    //       _ = isBottom |= v.isBottom
    //     } yield x -> v).toMap
    //     val newHeap = elem.heap ⊓ that.heap
    //     if (newHeap.isBottom || isBottom) Bot
    //     else Elem(true, newLocals, newGlobals, newHeap)

    // /** getters * */
    def apply(base: AbsValue, prop: AbsValue, check: Boolean): AbsValue = ???
    //   val compValue = base.comp(prop)
    //   val locValue = heap(base.loc, prop)
    //   val astValue = lookupAst(base.ast, prop)
    //   val strValue = lookupStr(base.str, prop)
    //   compValue ⊔ locValue ⊔ astValue ⊔ strValue
    // private def lookupAst(ast: AbsAst, prop: AbsValue): AbsValue =
    //   (ast.getSingle, prop.getSingle) match {
    //     case (FlatBot, _) | (_, FlatBot) => AbsValue.Bot
    //     case (FlatElem(AAst(ast)), FlatElem(ASimple(Str("parent")))) =>
    //       ast.parent.map(AbsValue(_)).getOrElse(AbsValue(Absent))
    //     case (
    //           FlatElem(AAst(syn: es.Syntactic)),
    //           FlatElem(ASimple(Str(propStr))),
    //         ) =>
    //       val es.Syntactic(name, _, rhsIdx, children) = syn
    //       val rhs = cfg.grammar.nameMap(name).rhsList(rhsIdx)
    //       rhs.getNtIndex(propStr).flatMap(children(_)) match
    //         case Some(child) => AbsValue(child)
    //         case _           => AbsValue.Bot
    //     case (FlatElem(AAst(syn: es.Syntactic)), FlatElem(AMath(n)))
    //         if n.isValidInt =>
    //       syn.children(n.toInt).map(AbsValue(_)).getOrElse(AbsValue(Absent))
    //     case (_: FlatElem[_], _: FlatElem[_]) => AbsValue.Bot
    //     case _                                => exploded("ast property access")
    //   }
    // private def lookupStr(str: AbsStr, prop: AbsValue): AbsValue =
    //   (str.getSingle, prop.getSingle) match {
    //     case (FlatBot, _) | (_, FlatBot) => AbsValue.Bot
    //     case (FlatElem(Str(str)), FlatElem(AMath(k))) =>
    //       AbsValue(CodeUnit(str(k.toInt)))
    //     case (FlatElem(Str(str)), FlatElem(ASimple(simple))) =>
    //       simple match
    //         case Str("length") => AbsValue(Math(BigDecimal.exact(str.length)))
    //         case Number(k)     => AbsValue(CodeUnit(str(k.toInt)))
    //         case _             => AbsValue.Bot
    //     case _ => AbsValue.codeUnit ⊔ AbsValue.math
    //   }
    def apply(part: Part): AbsObj = ??? // heap(loc)
    def lookupGlobal(x: Global): AbsValue = ???
    //   elem.globals.getOrElse(x, baseGlobals.getOrElse(x, AbsValue.Bot))

    /** setters */
    def update(x: Id, value: AbsValue): Elem = ???
    //   bottomCheck(value) {
    //     x match
    //       case x: Global if globals contains x =>
    //         copy(globals = globals + (x -> value))
    //       case x: Name if locals contains x =>
    //         copy(locals = locals + (x -> value))
    //       case x: Temp =>
    //         copy(locals = locals + (x -> value))
    //       case _ => Bot
    //   }
    def update(aloc: AbsValue, prop: AbsValue, value: AbsValue): Elem = ???
    //   bottomCheck(aloc, prop, value) {
    //     copy(heap = heap.update(aloc.loc, prop, value))
    //   }

    /** delete a property from a map */
    def delete(refV: AbsRefValue): Elem = ??? // refV match
    //   case AbsRefId(x) => error(s"cannot delete variable $x")
    //   case AbsRefProp(base, prop) =>
    //     bottomCheck(base, prop) {
    //       copy(heap = heap.delete(base.loc, prop))
    //     }

    // /** default value for bottom check */
    // given bottomValue: AbsValue = AbsValue.Bot

    /** object operators */
    def push(list: AbsValue, value: AbsValue, front: Boolean): Elem = ???
    //   bottomCheck(list, elem) {
    //     if (front) copy(heap = heap.prepend(list.loc, elem))
    //     else copy(heap = heap.append(list.loc, elem))
    //   }
    def remove(list: AbsValue, value: AbsValue): Elem = ???
    //   bottomCheck(list, elem) { copy(heap = heap.remove(list.loc, elem)) }
    def pop(list: AbsValue, front: Boolean): (AbsValue, Elem) = ???
    //   var v: AbsValue = AbsValue.Bot
    //   val st: Elem = bottomCheck(list.loc) {
    //     val (newV, newH) = heap.pop(list.loc, front)
    //     v ⊔= newV
    //     copy(heap = newH)
    //   }
    //   (v, st)

    def setType(v: AbsValue, tname: String): (AbsValue, Elem) = ???
    //   bottomCheck(v.loc) { (v, copy(heap = heap.setType(v.loc, tname))) }
    def copyObj(from: AbsValue, to: AllocSite): (AbsValue, Elem) = ???
    //   val locV = AbsValue(to)
    //   bottomCheck(from.loc) { (locV, copy(heap = heap.copyObj(from.loc)(to))) }
    def keys(
      v: AbsValue,
      intSorted: Boolean,
      to: AllocSite,
    ): (AbsValue, Elem) = ???
    //   val locV = AbsValue(to)
    //   bottomCheck(v.loc) {
    //     (locV, copy(heap = heap.keys(v.loc, intSorted)(to)))
    //   }
    def listConcat(ls: List[AbsValue], to: AllocSite): (AbsValue, Elem) = ???
    //   import AbsObj.*
    //   bottomCheck(ls) {
    //     val newList = ls.foldLeft(List[AbsValue]()) {
    //       case (acc, l) =>
    //         l.getSingle match
    //           case FlatElem(loc: Loc) =>
    //             elem(loc) match
    //               case KeyWiseList(vs) => acc ++ vs
    //               case _               => ??? // TODO
    //           case _ => ??? // TODO
    //     }
    //     allocList(newList, to)
    //   }
    def getChildren(
      ast: AbsValue,
      kindOpt: Option[AbsValue],
      to: AllocSite,
    ): (AbsValue, Elem) = ???
    //   (kindOpt.map(_.getSingle), ast.getSingle) match
    //     case (Some(FlatBot), _) | (_, FlatBot) => (AbsValue.Bot, Bot)
    //     case (Some(FlatTop), _) | (_, FlatTop) => exploded("EGetChildren")
    //     case (Some(FlatElem(AGrammar(name, _))), FlatElem(AAst(ast))) =>
    //       val vs = ast.getChildren(name).map(AbsValue(_))
    //       allocList(vs, to)
    //     case (None, FlatElem(AAst(syn: Syntactic))) =>
    //       val vs = syn.children.flatten.map(AbsValue(_))
    //       allocList(vs, to)
    //     case _ => (AbsValue.Bot, Bot)

    def allocMap(
      tname: String,
      pairs: List[(AbsValue, AbsValue)],
      to: AllocSite,
    ): (AbsValue, Elem) = ???
    //   val locV = AbsValue(to)
    //   bottomCheck(pairs.flatMap { case (k, v) => List(k, v) }) {
    //     (locV, copy(heap = heap.allocMap(tname, pairs)(to)))
    //   }
    def allocList(list: List[AbsValue], to: AllocSite): (AbsValue, Elem) = ???
    //   val locV = AbsValue(to)
    //   bottomCheck(list) { (locV, copy(heap = heap.allocList(list)(to))) }
    def allocSymbol(desc: AbsValue, to: AllocSite): (AbsValue, Elem) = ???
    //   val locV = AbsValue(to)
    //   val descV = BasicValueDomain(str = desc.str, undef = desc.undef)
    //   bottomCheck(descV) {
    //     (locV, copy(heap = heap.allocSymbol(descV)(to)))
    //   }
    def contains(
      list: AbsValue,
      value: AbsValue,
      field: Option[(Type, String)],
    ): AbsValue = ??? // field match
    //   case Some(_) => ??? // TODO
    //   case None    => heap.contains(list.loc, value)

    /** define global variables */
    def defineGlobal(pairs: (Global, AbsValue)*): Elem = ???
    //   bottomCheck(pairs.unzip._2) { copy(globals = globals ++ pairs) }

    /** define local variables */
    def defineLocal(pairs: (Local, AbsValue)*): Elem = ???
    //   bottomCheck(pairs.unzip._2) { copy(locals = locals ++ pairs) }

    /** singleton checks */
    override def isSingle: Boolean = ???
    //   super.isSingle && globals.forall(_._2.isSingle) && heap.isSingle

    /** singleton location checks */
    def isSingle(part: Part): Boolean = ??? // heap.isSingle(loc)

    /** find merged parts */
    def findMerged: Unit = ??? // {
    //   // visited locations
    //   var visited = Set[Part]()

    //   // auxiliary functions
    //   def auxValue(
    //     value: AbsValue,
    //     path: String,
    //     locPath: String = "",
    //   ): Unit = {
    //     if (!value.isBottom && !value.isSingle) {
    //       if (locPath == "") println(s"$path is merged: $value")
    //       else println(s"$path ($locPath) is merged: $value")
    //     }
    //     for (loc <- value.reachableParts) auxLoc(loc, path)
    //   }
    //   def auxLoc(loc: Loc, path: String): Unit = if (
    //     (
    //       !visited.contains(loc) &&
    //       heap.map.contains(loc)
    //     )
    //   ) {
    //     visited += loc
    //     heap(loc) match {
    //       case AbsObj.Bot =>
    //       case AbsObj.SymbolElem(desc) =>
    //         auxValue(desc, s"$path.desc", s"$loc.desc")
    //       case AbsObj.OrderedMap(_, map, _) =>
    //         for ((p, v) <- map) {
    //           auxValue(v, s"$path[$p]", s"$loc[$p]")
    //         }
    //       case AbsObj.KeyWiseList(values) =>
    //         for ((v, k) <- values.zipWithIndex) {
    //           auxValue(v, s"$path[$k]", s"$loc[$k]")
    //         }
    //       case AbsObj.NotSupportedElem(_, _) =>
    //       case obj => println(s"$path ($loc) is merged object: $obj")
    //     }
    //   }

    //   for ((x, v) <- locals) auxValue(v, s"local $x")
    //   for ((x, v) <- globals) auxValue(v, s"global $x")
    //   for (loc <- heap.map.keys if loc.isNamed) auxLoc(loc, s"$loc")
    //   for (loc <- heap.map.keys if !loc.isNamed) auxLoc(loc, s"<unreachable>")
    // }

    /** handle calls */
    def doCall: Elem = ??? // elem
    //   .copy(heap = heap.doCall)
    //   .garbageCollected
    def doProcStart(fixed: Set[Part]): Elem = ??? // elem
    //   .copy(heap = heap.doProcStart(fixed))
    //   .garbageCollected

    /** handle returns (elem: return states / to: caller states) */
    def doReturn(to: Elem, defs: Iterable[(Local, AbsValue)]): Elem =
      ??? // Elem(
    //   reachable = true,
    //   locals = to.locals ++ defs,
    //   globals = globals,
    //   heap = heap.doReturn(to.heap),
    // ).garbageCollected
    def doProcEnd(to: Elem, defs: (Local, AbsValue)*): Elem =
      doProcEnd(to, defs)
    def doProcEnd(to: Elem, defs: Iterable[(Local, AbsValue)]): Elem =
      ??? // Elem(
    //   reachable = true,
    //   locals = to.locals ++ defs,
    //   globals = globals,
    //   heap = heap.doProcEnd(to.heap),
    // ).garbageCollected

    // TODO garbage collection
    def garbageCollected: Elem = ??? // elem
    // // if (USE_GC) {
    // //   val unreachLocs = (heap.map.keySet -- reachableParts).filter(!_.isNamed)
    // //   copy(heap = heap.removeLocs(unreachLocs))
    // // } else elem

    /** get reachable locations */
    def reachableParts: Set[Part] = ??? // {
    //   var locs = Set[Part]()
    //   for ((_, v) <- locals) locs ++= v.reachableParts
    //   for ((_, v) <- globals) locs ++= v.reachableParts
    //   heap.reachableParts(locs)
    // }

    /** copy */
    def copied(
      locals: Map[Local, AbsValue] = Map(),
    ): Elem = elem.copy(locals = locals)

    /** conversion to string */
    def toString(detail: Boolean): String = ??? // {
    //   val app = Appender()
    //   given Rule[Elem] =
    //     if (detail) BasicStateDomain.rule else BasicStateDomain.shortRule
    //   app >> elem
    //   app.toString
    // }

    /** get string wth detailed shapes of locations */
    def getString(value: AbsValue): String = ??? // {
    //   val app = Appender()
    //   app >> value.toString
    //   val locs = value.reachableParts
    //   if (!locs.isEmpty) (app >> " @ ").wrap(for (loc <- locs) {
    //     val obj = heap(loc)
    //     app :> s"$loc -> " >> obj >> LINE_SEP
    //   })
    //   app.toString
    // }

    /** getters */
    def reachable: Boolean = ???
    def locals: Map[Local, AbsValue] = ???
    def globals: Map[Global, AbsValue] = ???
    // def heap: AbsHeap = ???
  }
}
