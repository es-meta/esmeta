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

  /** base globals */
  def baseGlobals: Map[Id, AbsValue]

  /** monad helper */
  val monad: StateMonad[Elem] = StateMonad[Elem]()

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
    def apply(x: Id, cp: ControlPoint, check: Boolean): AbsValue =
      val v = directLookup(x)
      if (cp.isBuiltin && AbsValue.absentTop ⊑ v)
        v.removeAbsent ⊔ AbsValue.undefTop
      else v
    def apply(base: AbsValue, prop: AbsValue, check: Boolean = true): AbsValue
    def apply(part: Part): AbsObj // TODO remove

    /** lookup variables */
    def directLookup(x: Id): AbsValue = x match
      case x: Local  => lookupLocal(x)
      case x: Global => lookupGlobal(x)
    def lookupLocal(x: Local): AbsValue =
      elem.locals.getOrElse(x, AbsValue.Bot)
    def lookupGlobal(x: Global): AbsValue

    /** existence checks */
    def exists(ref: AbsRefValue): AbsValue = ref match
      case AbsRefId(id)           => !directLookup(id).isAbsent
      case AbsRefProp(base, prop) => !elem(base, prop, check = false).isAbsent

    /** define global variables */
    def defineGlobal(pairs: (Global, AbsValue)*): Elem
    def defineLocal(pairs: (Local, AbsValue)*): Elem

    /** setters */
    def update(refV: AbsRefValue, value: AbsValue): Elem = refV match
      case AbsRefId(x)            => update(x, value)
      case AbsRefProp(base, prop) => update(base, prop, value)
    def update(x: Id, value: AbsValue): Elem
    def update(base: AbsValue, prop: AbsValue, value: AbsValue): Elem

    /** object operators */
    def delete(refV: AbsRefValue): Elem

    def push(list: AbsValue, value: AbsValue, front: Boolean): Elem
    def remove(list: AbsValue, value: AbsValue): Elem
    def pop(list: AbsValue, front: Boolean): (AbsValue, Elem)

    def setType(part: AbsValue, tname: String): (AbsValue, Elem)
    def copyObj(from: AbsValue, to: AllocSite): (AbsValue, Elem)
    def keys(
      part: AbsValue,
      intSorted: Boolean,
      to: AllocSite,
    ): (AbsValue, Elem)
    def listConcat(ls: List[AbsValue], to: AllocSite): (AbsValue, Elem)
    def getChildren(
      ast: AbsValue,
      kindOpt: Option[AbsValue],
      to: AllocSite,
    ): (AbsValue, Elem)

    def allocMap(
      tname: String,
      pairs: List[(AbsValue, AbsValue)],
      to: AllocSite,
    ): (AbsValue, Elem)
    def allocList(list: List[AbsValue], to: AllocSite): (AbsValue, Elem)
    def allocSymbol(desc: AbsValue, to: AllocSite): (AbsValue, Elem)
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
    def isSingle: Boolean = reachable && locals.forall(_._2.isSingle)
    def isSingle(part: Part): Boolean

    /** copy */
    def copied(
      locals: Map[Local, AbsValue] = Map(),
    ): Elem

    /** conversion to string */
    def toString(detail: Boolean): String

    /** get string with detailed shape of address partitions */
    def getString(value: AbsValue): String

    /** check bottom elements in abstract semantics */
    protected def bottomCheck(
      dom: domain.Domain[_] with Singleton,
    )(
      vs: dom.Elem*,
    )(f: => Elem): Elem = bottomCheck(dom)(vs)(f)
    protected def bottomCheck(
      dom: domain.Domain[_] with Singleton,
    )(
      vs: Iterable[dom.Elem],
    )(f: => Elem): Elem = if (elem.isBottom || vs.exists(_.isBottom)) Bot else f
    protected def bottomCheck(
      dom: domain.Domain[_] with Singleton,
    )(
      vs: dom.Elem*,
    )(f: => (AbsValue, Elem))(using default: AbsValue): (AbsValue, Elem) =
      bottomCheck(dom)(vs)(f)
    protected def bottomCheck(
      dom: domain.Domain[_] with Singleton,
    )(
      vs: Iterable[dom.Elem],
    )(f: => (AbsValue, Elem))(using default: AbsValue): (AbsValue, Elem) =
      if (elem.isBottom || vs.exists(_.isBottom)) (default, Bot)
      else f

    /** getters */
    def reachable: Boolean
    def locals: Map[Local, AbsValue]
    def globals: Map[Global, AbsValue]
    // def heap: AbsHeap = ???
  }
}
