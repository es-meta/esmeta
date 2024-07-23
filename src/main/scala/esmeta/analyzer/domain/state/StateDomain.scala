package esmeta.analyzer.domain.state

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.es.Initialize
import esmeta.ir.*
import esmeta.state.*
import esmeta.util.Appender.{*, given}
import esmeta.util.BaseUtils.*
import esmeta.util.StateMonad

trait StateDomainDecl { self: Self =>

  /** abstract state domain */
  trait StateDomain extends Domain[State] {

    /** empty state */
    def Empty: Elem

    /** monad helper */
    val monad: StateMonad[Elem] = StateMonad[Elem]()

    /** set bases */
    def setBase(init: Initialize): Unit

    /** abstract state interfaces */
    extension (elem: Elem) {

      /** getters */
      def get(
        rt: AbsRefTarget,
        cp: ControlPoint,
      ): AbsValue = rt match
        case AbsVarTarget(x)             => elem.get(x, cp)
        case AbsFieldTarget(base, field) => elem.get(base, field)

      /** getters with identifiers */
      def get(x: Var, cp: ControlPoint): AbsValue =
        val v = directLookup(x)
        if (cp.isBuiltin && AbsValue.absentTop ⊑ v)
          v.removeAbsent ⊔ AbsValue.undefTop
        else v

      /** getters with bases and fields */
      def get(base: AbsValue, field: AbsValue): AbsValue

      /** getters with an address partition */
      def get(part: Part): AbsObj

      /** lookup variables */
      def directLookup(x: Var): AbsValue = x match
        case x: Local  => lookupLocal(x)
        case x: Global => lookupGlobal(x)

      /** lookup local variables */
      def lookupLocal(x: Local): AbsValue =
        elem.locals.getOrElse(x, AbsValue.Bot)

      /** lookup global variables */
      def lookupGlobal(x: Global): AbsValue

      /** existence checks */
      def exists(rt: AbsRefTarget): AbsValue = rt match
        case AbsVarTarget(x) => !directLookup(x).isAbsent
        case AbsFieldTarget(base, field) =>
          !elem.get(base, field).isAbsent

      /** define local variables */
      def defineLocal(pairs: (Local, AbsValue)*): Elem

      /** define global variables */
      def defineGlobal(pairs: (Global, AbsValue)*): Elem

      /** setter with reference values */
      def update(rt: AbsRefTarget, value: AbsValue): Elem = rt match
        case AbsVarTarget(x)             => update(x, value)
        case AbsFieldTarget(base, field) => update(base, field, value)

      /** identifier setter */
      def update(x: Var, value: AbsValue): Elem

      /** field setter */
      def update(base: AbsValue, field: AbsValue, value: AbsValue): Elem

      /** deletion with reference values */
      def delete(rt: AbsRefTarget): Elem

      /** push values to a list */
      def push(list: AbsValue, value: AbsValue, front: Boolean): Elem

      /** pop a value in a list */
      def pop(list: AbsValue, front: Boolean): (AbsValue, Elem)

      /** set a type to an address partition */
      def setType(part: AbsValue, tname: String): (AbsValue, Elem)

      /** copy object */
      def copyObj(to: AllocSite, from: AbsValue): (AbsValue, Elem)

      /** get object keys */
      def keys(
        to: AllocSite,
        part: AbsValue,
        intSorted: Boolean,
      ): (AbsValue, Elem)

      /** list concatenation */
      def concat(
        to: AllocSite,
        ls: Iterable[AbsValue] = Nil,
      ): (AbsValue, Elem)

      /** get childeren of AST */
      def getChildren(
        to: AllocSite,
        ast: AbsValue,
      ): (AbsValue, Elem)

      /** get items of AST */
      def getItems(
        to: AllocSite,
        nt: AbsValue,
        ast: AbsValue,
      ): (AbsValue, Elem)

      /** allocation of map with address partitions */
      def allocMap(
        to: AllocSite,
        pairs: Iterable[(AbsValue, AbsValue)],
      ): (AbsValue, Elem)

      /** allocation of record with address partitions */
      def allocRecord(
        to: AllocSite,
        tname: String,
        pairs: Iterable[(String, AbsValue)],
      ): (AbsValue, Elem)

      /** allocation of list with address partitions */
      def allocList(
        to: AllocSite,
        list: Iterable[AbsValue] = Nil,
      ): (AbsValue, Elem)

      /** allocation of symbol with address partitions */
      def allocSymbol(to: AllocSite, desc: AbsValue): (AbsValue, Elem)

      /** check contains */
      def contains(list: AbsValue, value: AbsValue): AbsValue

      /** find merged parts */
      def findMerged: Unit

      /** handle calls */
      def doCall: Elem
      def doProcStart(fixed: Set[Part]): Elem

      /** handle returns (elem: return states / to: caller states) */
      def doReturn(to: Elem, defs: (Local, AbsValue)*): Elem =
        doReturn(to, defs)
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
      def getString(detail: Boolean): String

      /** get string with detailed shape of address partitions */
      def getString(value: AbsValue): String

      /** getters */
      def reachable: Boolean
      def locals: Map[Local, AbsValue]
      def globals: Map[Global, AbsValue]
      def heap: AbsHeap

      /** check bottom elements in abstract semantics */
      protected def bottomCheck[A](dom: Domain[A])(
        vs: dom.Elem*,
      )(f: => Elem): Elem = elem.bottomCheck(dom)(vs)(f)
      protected def bottomCheck[A](dom: Domain[_])(
        vs: Iterable[dom.Elem],
      )(f: => Elem): Elem =
        if (elem.isBottom || vs.exists(_.isBottom)) Bot else f
      protected def bottomCheck[A](dom: Domain[A])(
        vs: dom.Elem*,
      )(f: => (AbsValue, Elem)): (AbsValue, Elem) = elem.bottomCheck(dom)(vs)(f)
      protected def bottomCheck[A](dom: Domain[A])(
        vs: Iterable[dom.Elem],
      )(f: => (AbsValue, Elem)): (AbsValue, Elem) =
        if (elem.isBottom || vs.exists(_.isBottom)) (AbsValue.Bot, Bot)
        else f
    }
  }
}
