package esmeta.analyzer.domain.state

import esmeta.LINE_SEP
import esmeta.analyzer.*
import esmeta.analyzer.Config.*
import esmeta.analyzer.domain.*
import esmeta.state.*
import esmeta.ir.*
import esmeta.es
import esmeta.es.*
import esmeta.util.*
import esmeta.util.Appender
import esmeta.util.Appender.{*, given}
import esmeta.util.BaseUtils.*

/** type domain for states */
object TypeDomain extends state.Domain {

  /** elements */
  case class Elem() extends Appendable

  /** top element */
  lazy val Top: Elem = exploded("top abstract state")

  /** bottom element */
  val Bot: Elem = Elem()

  /** empty element */
  val Empty: Elem = ???

  /** abstraction functions */
  def alpha(xs: Iterable[State]): Elem = ???

  /** appender */
  given rule: Rule[Elem] = ???

  /** simpler appender */
  val shortRule: Rule[Elem] = ???

  /** element interfaces */
  extension (elem: Elem) {

    /** partial order */
    def ⊑(that: Elem): Boolean = ???

    /** join operator */
    def ⊔(that: Elem): Elem = ???

    /** meet operator */
    override def ⊓(that: Elem): Elem = ???

    /** getters with bases and properties */
    def get(base: AbsValue, prop: AbsValue, check: Boolean): AbsValue = ???

    /** getters with an address partition */
    def get(part: Part): AbsObj = ???

    /** lookup global variables */
    def lookupGlobal(x: Global): AbsValue = ???

    /** identifier setter */
    def update(x: Id, value: AbsValue): Elem = ???

    /** property setter */
    def update(base: AbsValue, prop: AbsValue, value: AbsValue): Elem = ???

    /** deletion wiht reference values */
    def delete(refV: AbsRefValue): Elem = ???

    /** push values to a list */
    def push(list: AbsValue, value: AbsValue, front: Boolean): Elem = ???

    /** remove a value in a list */
    def remove(list: AbsValue, value: AbsValue): Elem = ???

    /** pop a value in a list */
    def pop(list: AbsValue, front: Boolean): (AbsValue, Elem) = ???

    /** set a type to an address partition */
    def setType(v: AbsValue, tname: String): (AbsValue, Elem) = ???

    /** copy object */
    def copyObj(to: AllocSite, from: AbsValue): (AbsValue, Elem) = ???

    /** get object keys */
    def keys(
      to: AllocSite,
      v: AbsValue,
      intSorted: Boolean,
    ): (AbsValue, Elem) = ???

    /** list concatenation */
    def concat(
      to: AllocSite,
      lists: Iterable[AbsValue] = Nil,
    ): (AbsValue, Elem) = ???

    /** get childeren of AST */
    def getChildren(
      to: AllocSite,
      ast: AbsValue,
      kindOpt: Option[AbsValue],
    ): (AbsValue, Elem) = ???

    /** allocation of map with address partitions */
    def allocMap(
      to: AllocSite,
      tname: String,
      pairs: Iterable[(AbsValue, AbsValue)],
    ): (AbsValue, Elem) = ???

    /** allocation of list with address partitions */
    def allocList(
      to: AllocSite,
      list: Iterable[AbsValue] = Nil,
    ): (AbsValue, Elem) = ???

    /** allocation of symbol with address partitions */
    def allocSymbol(to: AllocSite, desc: AbsValue): (AbsValue, Elem) = ???

    /** check contains */
    def contains(
      list: AbsValue,
      value: AbsValue,
      field: Option[(Type, String)],
    ): AbsValue = ???

    /** define global variables */
    def defineGlobal(pairs: (Global, AbsValue)*): Elem = ???

    /** define local variables */
    def defineLocal(pairs: (Local, AbsValue)*): Elem = ???

    /** singleton checks */
    override def isSingle: Boolean = ???

    /** singleton address partition checks */
    def isSingle(part: Part): Boolean = ???

    /** find merged parts */
    def findMerged: Unit = ???

    /** handle calls */
    def doCall: Elem = ???
    def doProcStart(fixed: Set[Part]): Elem = ???

    /** handle returns (elem: return states / to: caller states) */
    def doReturn(to: Elem, defs: Iterable[(Local, AbsValue)]): Elem = ???
    def doProcEnd(to: Elem, defs: (Local, AbsValue)*): Elem = ???
    def doProcEnd(to: Elem, defs: Iterable[(Local, AbsValue)]): Elem = ???

    /** garbage collection */
    def garbageCollected: Elem = ???

    /** get reachable address partitions */
    def reachableParts: Set[Part] = ???

    /** copy */
    def copied(locals: Map[Local, AbsValue] = Map()): Elem = ???

    /** get string */
    def getString(detail: Boolean): String = ???

    /** get string wth detailed shapes of locations */
    def getString(value: AbsValue): String = ???

    /** getters */
    def reachable: Boolean = ???
    def locals: Map[Local, AbsValue] = ???
    def globals: Map[Global, AbsValue] = ???
    def heap: AbsHeap = ???
  }
}
