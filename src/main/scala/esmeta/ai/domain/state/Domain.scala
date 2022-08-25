package esmeta.ai.domain.state

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.state.*
import esmeta.ir.*
import esmeta.util.Appender.{*, given}
import esmeta.util.BaseUtils.*
import esmeta.util.StateMonad

/** abstract state domain */
trait Domain extends domain.Domain[State] {

  /** empty state */
  def Empty: Elem

  /** monad helper */
  val monad: StateMonad[Elem] = StateMonad[Elem]()

  /** simpler appender */
  val shortRule: Rule[Elem]

  /** abstract state interfaces */
  extension (elem: Elem) {

    /** getters */
    def apply(
      rv: AbsRefValue,
      cp: ControlPoint,
      check: Boolean,
    ): AbsValue = rv match
      case AbsRefId(x)            => elem(x, cp, check)
      case AbsRefProp(base, prop) => elem(base, prop, check)

    /** getters with identifiers */
    def apply(x: Id, cp: ControlPoint, check: Boolean): AbsValue =
      val v = directLookup(x)
      if (cp.isBuiltin && AbsValue.absentTop ⊑ v)
        v.removeAbsent ⊔ AbsValue.undefTop
      else v

    /** getters with bases and properties */
    def apply(base: AbsValue, prop: AbsValue, check: Boolean = true): AbsValue

    /** getters with an address partition */
    def apply(part: Part): AbsObj

    /** lookup variables */
    def directLookup(x: Id): AbsValue = x match
      case x: Local  => lookupLocal(x)
      case x: Global => lookupGlobal(x)

    /** lookup local variables */
    def lookupLocal(x: Local): AbsValue =
      elem.locals.getOrElse(x, AbsValue.Bot)

    /** lookup global variables */
    def lookupGlobal(x: Global): AbsValue

    /** existence checks */
    def exists(ref: AbsRefValue): AbsValue = ref match
      case AbsRefId(id)           => !directLookup(id).isAbsent
      case AbsRefProp(base, prop) => !elem(base, prop, check = false).isAbsent

    /** define local variables */
    def defineLocal(pairs: (Local, AbsValue)*): Elem

    /** define global variables */
    def defineGlobal(pairs: (Global, AbsValue)*): Elem

    /** setter with reference values */
    def update(refV: AbsRefValue, value: AbsValue): Elem = refV match
      case AbsRefId(x)            => update(x, value)
      case AbsRefProp(base, prop) => update(base, prop, value)

    /** identifier setter */
    def update(x: Id, value: AbsValue): Elem

    /** property setter */
    def update(base: AbsValue, prop: AbsValue, value: AbsValue): Elem

    /** deletion wiht reference values */
    def delete(refV: AbsRefValue): Elem

    /** push values to a list */
    def push(list: AbsValue, value: AbsValue, front: Boolean): Elem

    /** remove a value in a list */
    def remove(list: AbsValue, value: AbsValue): Elem

    /** pop a value in a list */
    def pop(list: AbsValue, front: Boolean): (AbsValue, Elem)

    /** set a type to an address partition */
    def setType(part: AbsValue, tname: String): (AbsValue, Elem)

    /** copy object */
    def copyObj(from: AbsValue, to: AllocSite): (AbsValue, Elem)

    /** get object keys */
    def keys(
      part: AbsValue,
      intSorted: Boolean,
      to: AllocSite,
    ): (AbsValue, Elem)

    /** list concatenation */
    def concat(ls: Iterable[AbsValue], to: AllocSite): (AbsValue, Elem)

    /** get childeren of AST */
    def getChildren(
      ast: AbsValue,
      kindOpt: Option[AbsValue],
      to: AllocSite,
    ): (AbsValue, Elem)

    /** allocation of map with address partitions */
    def allocMap(
      to: AllocSite,
      tname: String,
      pairs: Iterable[(AbsValue, AbsValue)],
    ): (AbsValue, Elem)

    /** allocation of list with address partitions */
    def allocList(
      to: AllocSite,
      list: Iterable[AbsValue] = Nil,
    ): (AbsValue, Elem)

    /** allocation of symbol with address partitions */
    def allocSymbol(to: AllocSite, desc: AbsValue): (AbsValue, Elem)

    /** check contains */
    def contains(
      list: AbsValue,
      value: AbsValue,
      field: Option[(Type, String)],
    ): AbsValue

    /** find merged parts */
    def findMerged: Unit

    /** handle calls */
    def doCall: Elem
    def doProcStart(fixed: Set[Part]): Elem

    /** handle returns (elem: return states / to: caller states) */
    def doReturn(to: Elem, defs: (Local, AbsValue)*): Elem = doReturn(to, defs)
    def doReturn(to: Elem, defs: Iterable[(Local, AbsValue)]): Elem
    def doProcEnd(to: Elem, defs: (Local, AbsValue)*): Elem
    def doProcEnd(to: Elem, defs: Iterable[(Local, AbsValue)]): Elem
    def garbageCollected: Elem

    /** get reachable address partitions */
    def reachableParts: Set[Part]

    /** singleton checks */
    def isSingle: Boolean

    /** singleton address partition checks */
    def isSingle(part: Part): Boolean

    /** copy */
    def copied(
      locals: Map[Local, AbsValue] = Map(),
    ): Elem

    /** conversion to string */
    def toString(detail: Boolean): String

    /** get string with detailed shape of address partitions */
    def getString(value: AbsValue): String

    /** getters */
    def reachable: Boolean
    def locals: Map[Local, AbsValue]
    def globals: Map[Global, AbsValue]
    def heap: AbsHeap

    /** check bottom elements in abstract semantics */
    protected def bottomCheck[A](dom: domain.Domain[A])(
      vs: dom.Elem*,
    )(f: => Elem): Elem = elem.bottomCheck(dom)(vs)(f)
    protected def bottomCheck[A](dom: domain.Domain[_])(
      vs: Iterable[dom.Elem],
    )(f: => Elem): Elem =
      if (elem.isBottom || vs.exists(_.isBottom)) Bot else f
    protected def bottomCheck[A](dom: domain.Domain[A])(
      vs: dom.Elem*,
    )(f: => (AbsValue, Elem)): (AbsValue, Elem) = elem.bottomCheck(dom)(vs)(f)
    protected def bottomCheck[A](dom: domain.Domain[A])(
      vs: Iterable[dom.Elem],
    )(f: => (AbsValue, Elem)): (AbsValue, Elem) =
      if (elem.isBottom || vs.exists(_.isBottom)) (AbsValue.Bot, Bot)
      else f
  }
}
