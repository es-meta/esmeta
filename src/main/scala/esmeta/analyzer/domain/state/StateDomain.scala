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
import scala.annotation.targetName // TODO remove this

/** abstract states */
trait StateDomain extends Domain { stateDomain: StateDomain =>
  // bottom element
  val Bot: Elem = stateDomain(false, Map(), Map())

  // empty state
  val Empty: Elem = stateDomain(true, Map(), Map())

  // monad helper
  val monad: StateMonad[Elem] = new StateMonad[Elem]

  // base globals
  lazy val baseGlobals: Map[Global, Value] = new js.Initialize(cfg).initGlobal
  lazy val base: Map[Id, AbsValue] = (for {
    (x, v) <- baseGlobals.toList
  } yield x -> AbsValue(v)).toMap

  // constructors
  def apply(
    reachable: Boolean,
    locals: Map[Local, AbsValue],
    globals: Map[Global, AbsValue],
  ): Elem

  // extractors
  def unapply(elem: Elem) = Some(
    (
      elem.reachable,
      elem.locals,
      elem.globals,
    ),
  )

  // elements
  type Elem <: StateElemTrait
  trait StateElemTrait extends super.ElemTrait { this: Elem =>
    val reachable: Boolean
    val locals: Map[Local, AbsValue]
    val globals: Map[Global, AbsValue]

    // partial order
    override def isBottom = !this.reachable
    def ⊑(that: Elem): Boolean = (this, that) match {
      case _ if this.isBottom => true
      case _ if that.isBottom => false
      case (
            stateDomain(_, llocals, lglobals),
            stateDomain(_, rlocals, rglobals),
          ) => {
        val localsB = (llocals.keySet ++ rlocals.keySet).forall(x => {
          this.lookupLocal(x) ⊑ that.lookupLocal(x)
        })
        val globalsB = (lglobals.keySet ++ rglobals.keySet).forall(x => {
          this.lookupGlobal(x) ⊑ that.lookupGlobal(x)
        })
        localsB && globalsB
      }
    }

    // join operator
    def ⊔(that: Elem): Elem = (this, that) match {
      case _ if this.isBottom => that
      case _ if that.isBottom => this
      case (
            stateDomain(_, llocals, lglobals),
            stateDomain(_, rlocals, rglobals),
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
        stateDomain(true, newLocals, newGlobals)
      }
    }

    // meet operator
    def ⊓(that: Elem): Elem = (this, that) match {
      case _ if this.isBottom || that.isBottom => Bot
      case (
            stateDomain(_, llocals, lglobals),
            stateDomain(_, rlocals, rglobals),
          ) => {
        val newLocals = (for {
          x <- (llocals.keySet intersect rlocals.keySet).toList
          v = this.lookupLocal(x) ⊓ that.lookupLocal(x)
          if !v.isBottom
        } yield x -> v).toMap
        val newGlobals = (for {
          x <- (lglobals.keySet intersect rglobals.keySet).toList
          v = this.lookupGlobal(x) ⊓ that.lookupGlobal(x)
          if !v.isBottom
        } yield x -> v).toMap
        val isBottom = newLocals.isEmpty || newGlobals.isEmpty
        stateDomain(!isBottom, newLocals, newGlobals)
      }
    }

    // getters
    def apply(rv: AbsRefValue, cp: ControlPoint): AbsValue = rv match
      case AbsRefId(x)            => this(x, cp)
      case AbsRefProp(base, prop) => this(base, prop)
    def apply(x: Id, cp: ControlPoint): AbsValue =
      val v = directLookup(x)
      if (cp.isBuiltin && AbsValue.absent ⊑ v) v.removeAbsent ⊔ AbsValue.undef
      else v
    def apply(base: AbsValue, prop: AbsValue): AbsValue
    // def apply(comp: AbsComp, prop: AbsValue): AbsValue = comp(prop)
    // @targetName("apply_loc")
    // def apply(loc: AbsLoc, prop: AbsValue): AbsValue
    // @targetName("apply_ast")
    // def apply(ast: AbsAst, prop: AbsValue): AbsValue
    // @targetName("apply_str")
    // def apply(str: AbsStr, prop: AbsValue): AbsValue
    def apply(loc: Loc): AbsObj

    // lookup variables
    def directLookup(x: Id): AbsValue = x match
      case x: Local  => lookupLocal(x)
      case x: Global => lookupGlobal(x)
    def lookupLocal(x: Local): AbsValue =
      this.locals.getOrElse(x, AbsValue.Bot)
    def lookupGlobal(x: Global): AbsValue =
      this.globals.getOrElse(x, base.getOrElse(x, AbsValue.Bot))

    // existence checks
    def exists(ref: AbsRefValue): AbsBool = ref match
      case AbsRefId(id)           => !directLookup(id).isAbsent
      case AbsRefProp(base, prop) => !this(base, prop).isAbsent

    // define global variables
    def defineGlobal(pairs: (Global, AbsValue)*): Elem
    def defineLocal(pairs: (Local, AbsValue)*): Elem

    // setters
    def update(refV: AbsRefValue, value: AbsValue): Elem = refV match
      case AbsRefId(x)            => update(x, value)
      case AbsRefProp(base, prop) => update(base, prop, value)
    def update(x: Id, value: AbsValue): Elem
    def update(base: AbsValue, prop: AbsValue, value: AbsValue): Elem

    // object operators
    def delete(refV: AbsRefValue): Elem
    def append(loc: AbsLoc, value: AbsValue): Elem
    def prepend(loc: AbsLoc, value: AbsValue): Elem
    def remove(loc: AbsLoc, value: AbsValue): Elem
    def pop(loc: AbsLoc, front: Boolean): (AbsValue, Elem)
    def copyObj(from: AbsLoc)(to: AllocSite): Elem
    def keys(loc: AbsLoc, intSorted: Boolean)(to: AllocSite): Elem
    def allocMap(tname: String, pairs: List[(AbsValue, AbsValue)])(
      to: AllocSite,
    ): Elem
    def allocList(list: List[AbsValue])(to: AllocSite): Elem
    def allocSymbol(desc: AbsValue)(to: AllocSite): (AbsValue, Elem)
    def setType(loc: AbsLoc, tname: String): Elem
    def contains(loc: AbsLoc, value: AbsValue): AbsBool

    // find merged parts
    def findMerged: Unit

    // handle calls
    def doCall: Elem
    def doProcStart(fixed: Set[Loc]): Elem

    // handle returns (this: return states / to: caller states)
    def doReturn(to: Elem, defs: (Local, AbsValue)*): Elem
    def doReturn(to: Elem, defs: Iterable[(Local, AbsValue)]): Elem
    def doProcEnd(to: Elem, defs: (Local, AbsValue)*): Elem
    def doProcEnd(to: Elem, defs: Iterable[(Local, AbsValue)]): Elem
    def garbageCollected: Elem

    // get reachable locations
    def reachableLocs: Set[Loc]

    // singleton checks
    def isSingle: Boolean = reachable &&
      locals.forall(_._2.isSingle) &&
      globals.forall(_._2.isSingle)
    def isSingle(loc: Loc): Boolean

    // copy
    def copied(
      locals: Map[Local, AbsValue] = Map(),
    ): Elem

    // conversion to string
    def toString(detail: Boolean): String

    // get string with detailed shape of locations
    def getString(value: AbsValue): String

    // check bottom elements in abstract semantics
    protected def bottomCheck(vs: Domain#ElemTrait*)(f: => Elem): Elem =
      bottomCheck(vs)(f)
    protected def bottomCheck(
      vs: Iterable[Domain#ElemTrait],
    )(f: => Elem): Elem =
      if (this.isBottom || vs.exists(_.isBottom)) Bot
      else f
    protected def bottomCheck[T](vs: Domain#ElemTrait*)(
      default: T,
      f: => (T, Elem),
    ): (T, Elem) =
      bottomCheck(vs)(default, f)
    protected def bottomCheck[T](
      vs: Iterable[Domain#ElemTrait],
    )(default: T, f: => (T, Elem)): (T, Elem) =
      if (this.isBottom || vs.exists(_.isBottom)) (default, Bot)
      else f
  }
}
