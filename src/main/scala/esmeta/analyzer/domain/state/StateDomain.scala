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

/** abstract states */
trait StateDomain extends Domain {

  /** bottom element */
  val Bot: Elem

  /** empty state */
  val Empty: Elem

  /** base globals */
  lazy val baseGlobals: Map[Id, AbsValue]

  /** monad helper */
  val monad: StateMonad[Elem] = new StateMonad[Elem]

  /** elements */
  type Elem <: StateElemTrait
  trait StateElemTrait extends super.ElemTrait { this: Elem =>
    val reachable: Boolean
    val locals: Map[Local, AbsValue]

    // partial order
    override def isBottom = !this.reachable
    def ⊑(that: Elem): Boolean = (this, that) match
      case _ if this.isBottom => true
      case _ if that.isBottom => false
      case (l, r) =>
        (l.locals.keySet ++ r.locals.keySet).forall(x => {
          this.lookupLocal(x) ⊑ that.lookupLocal(x)
        })

    /** join operator */
    def ⊔(that: Elem): Elem

    /** meet operator */
    def ⊓(that: Elem): Elem

    /** getters */
    def apply(rv: AbsRefValue, cp: ControlPoint): AbsValue = rv match
      case AbsRefId(x)            => this(x, cp)
      case AbsRefProp(base, prop) => this(base, prop)
    def apply(x: Id, cp: ControlPoint): AbsValue =
      val v = directLookup(x)
      if (cp.isBuiltin && AbsValue.absent ⊑ v) v.removeAbsent ⊔ AbsValue.undef
      else v
    def apply(base: AbsValue, prop: AbsValue): AbsValue
    def apply(loc: Loc): AbsObj // TODO remove

    /** lookup variables */
    def directLookup(x: Id): AbsValue = x match
      case x: Local  => lookupLocal(x)
      case x: Global => lookupGlobal(x)
    def lookupLocal(x: Local): AbsValue =
      this.locals.getOrElse(x, AbsValue.Bot)
    def lookupGlobal(x: Global): AbsValue

    /** existence checks */
    def exists(ref: AbsRefValue): AbsValue = ref match
      case AbsRefId(id)           => !directLookup(id).isAbsent
      case AbsRefProp(base, prop) => !this(base, prop).isAbsent

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

    def push(list: AbsValue, elem: AbsValue, front: Boolean): Elem
    def remove(list: AbsValue, elem: AbsValue): Elem
    def pop(list: AbsValue, front: Boolean): (AbsValue, Elem)

    def setType(loc: AbsValue, tname: String): (AbsValue, Elem)
    def copyObj(from: AbsValue, to: AllocSite): (AbsValue, Elem)
    def keys(loc: AbsValue, intSorted: Boolean, to: AllocSite): (AbsValue, Elem)
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
    def doProcStart(fixed: Set[Loc]): Elem

    /** handle returns (this: return states / to: caller states) */
    def doReturn(to: Elem, defs: (Local, AbsValue)*): Elem = doReturn(to, defs)
    def doReturn(to: Elem, defs: Iterable[(Local, AbsValue)]): Elem
    def doProcEnd(to: Elem, defs: (Local, AbsValue)*): Elem
    def doProcEnd(to: Elem, defs: Iterable[(Local, AbsValue)]): Elem
    def garbageCollected: Elem

    /** get reachable locations */
    def reachableLocs: Set[Loc]

    /** singleton checks */
    def isSingle: Boolean = reachable && locals.forall(_._2.isSingle)
    def isSingle(loc: Loc): Boolean

    /** copy */
    def copied(
      locals: Map[Local, AbsValue] = Map(),
    ): Elem

    /** conversion to string */
    def toString(detail: Boolean): String

    /** get string with detailed shape of locations */
    def getString(value: AbsValue): String

    /** check bottom elements in abstract semantics */
    protected def bottomCheck(vs: Domain#ElemTrait*)(f: => Elem): Elem =
      bottomCheck(vs)(f)
    protected def bottomCheck(
      vs: Iterable[Domain#ElemTrait],
    )(f: => Elem): Elem =
      if (this.isBottom || vs.exists(_.isBottom)) Bot
      else f
    protected def bottomCheck[T <: ValueDomain#Elem](vs: Domain#ElemTrait*)(
      f: => (T, Elem),
    )(using default: T): (T, Elem) = bottomCheck(vs)(f)
    protected def bottomCheck[T <: ValueDomain#Elem](
      vs: Iterable[Domain#ElemTrait],
    )(f: => (T, Elem))(using default: T): (T, Elem) =
      if (this.isBottom || vs.exists(_.isBottom)) (default, Bot)
      else f
  }
}
