package esmeta.analyzer.domain.state

import esmeta.LINE_SEP
import esmeta.analyzer.*
import esmeta.analyzer.Config.*
import esmeta.analyzer.domain.*
import esmeta.state.*
import esmeta.ir.{*, given}
import esmeta.es
import esmeta.es.*
import esmeta.ty.*
import esmeta.ty.util.Stringifier.{*, given}
import esmeta.util.*
import esmeta.util.Appender
import esmeta.util.Appender.{*, given}
import esmeta.util.BaseUtils.*

/** type domain for states */
object TypeDomain extends state.Domain {

  /** elements */
  case class Elem(
    reachable: Boolean = false,
    locals: Map[Local, AbsValue] = Map(),
  ) extends Appendable

  /** top element */
  lazy val Top: Elem = exploded("top abstract state")

  /** set bases */
  def setBase(init: Initialize): Unit = base = for {
    (x, (_, t)) <- init.initTypedGlobal.toMap
  } yield x -> AbsValue(t)
  private var base: Map[Global, AbsValue] = Map()

  /** bottom element */
  val Bot: Elem = Elem()

  /** empty element */
  val Empty: Elem = Elem(reachable = true)

  /** abstraction functions */
  def alpha(xs: Iterable[State]): Elem = Top

  /** appender */
  given rule: Rule[Elem] = mkRule(true)

  /** simpler appender */
  private val shortRule: Rule[Elem] = mkRule(false)

  /** element interfaces */
  extension (elem: Elem) {

    /** bottom check */
    override def isBottom = !elem.reachable

    /** partial order */
    def ⊑(that: Elem): Boolean = (elem, that) match
      case _ if elem.isBottom => true
      case _ if that.isBottom => false
      case (Elem(_, llocals), Elem(_, rlocals)) =>
        (llocals.keySet ++ rlocals.keySet).forall(x => {
          elem.lookupLocal(x) ⊑ that.lookupLocal(x)
        })

    /** join operator */
    def ⊔(that: Elem): Elem = (elem, that) match
      case _ if elem.isBottom => that
      case _ if that.isBottom => elem
      case (l, r) =>
        val newLocals = (for {
          x <- (l.locals.keySet ++ r.locals.keySet).toList
          v = elem.lookupLocal(x) ⊔ that.lookupLocal(x)
        } yield x -> v).toMap
        Elem(true, newLocals)

    /** meet operator */
    override def ⊓(that: Elem): Elem = (elem, that) match
      case _ if elem.isBottom || that.isBottom => Bot
      case (l, r) =>
        val newLocals = (for {
          x <- (l.locals.keySet ++ r.locals.keySet).toList
          v = elem.lookupLocal(x) ⊓ that.lookupLocal(x)
        } yield x -> v).toMap
        Elem(true, newLocals)

    /** getters with bases and properties */
    def get(base: AbsValue, prop: AbsValue): AbsValue =
      val baseTy = base.toTy
      val propTy = prop.toTy
      AbsValue(
        lookupComp(baseTy.comp, propTy) |
        lookupAst(baseTy.astValue, propTy) |
        lookupStr(baseTy.str, propTy) |
        lookupList(baseTy.list, propTy) |
        lookupName(baseTy.names, propTy) |
        lookupRecord(baseTy.record, propTy) |
        lookupSymbol(baseTy.symbol, propTy) |
        lookupSubMap(baseTy.subMap, propTy),
      )

    /** getters with an address partition */
    def get(part: Part): AbsObj = ???

    /** lookup global variables */
    def lookupGlobal(x: Global): AbsValue = ???

    /** identifier setter */
    def update(x: Id, value: AbsValue): Elem = x match
      case x: Local  => defineLocal(x -> value)
      case x: Global =>
        // TODO if (value !⊑ baseGlobals(x))
        //   logger.warn(s"invalid global variable update: $x = $value")
        elem

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
    def defineLocal(pairs: (Local, AbsValue)*): Elem =
      elem.copy(locals = locals ++ pairs)

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
    def getString(detail: Boolean): String = elem.toString

    /** get string wth detailed shapes of locations */
    def getString(value: AbsValue): String = value.toString

    /** getters */
    def reachable: Boolean = elem.reachable
    def locals: Map[Local, AbsValue] = locals
    def globals: Map[Global, AbsValue] = base
    def heap: AbsHeap = AbsHeap.Bot
  }

  // appender generator
  private def mkRule(detail: Boolean): Rule[Elem] = (app, elem) =>
    if (!elem.isBottom) {
      val irStringifier = IRElem.getStringifier(detail, false)
      import irStringifier.given
      given Rule[Map[Local, AbsValue]] = sortedMapRule(sep = ": ")
      app >> elem.locals >> LINE_SEP
    } else app >> "⊥"

  // completion record lookup
  lazy val constTyForAbruptTarget =
    CONSTT_BREAK | CONSTT_CONTINUE | CONSTT_RETURN | CONSTT_THROW
  private def lookupComp(comp: CompTy, prop: ValueTy): ValueTy =
    val str = prop.str
    val normal = !comp.normal.isBottom
    val abrupt = comp.abrupt
    var res = ValueTy()
    if (str contains "Value")
      if (normal) res |= ValueTy(pureValue = comp.normal)
      if (comp.abrupt) res |= ESValueT | CONSTT_EMPTY
    if (str contains "Target")
      if (normal) res |= CONSTT_EMPTY
      if (comp.abrupt) res |= StrTopT | CONSTT_EMPTY
    if (str contains "Type")
      if (normal) res |= CONSTT_NORMAL
      if (comp.abrupt) res |= constTyForAbruptTarget
    res

  // AST lookup
  private def lookupAst(ast: BSet[String], prop: ValueTy): ValueTy =
    var res = ValueTy()
    ast match
      case Inf => ???
      case Fin(set) =>
        for {
          name <- set
        } ???
    res

  // string lookup
  private def lookupStr(str: BSet[String], prop: ValueTy): ValueTy =
    val str = prop.str
    val math = prop.math
    var res = ValueTy()
    if (str contains "length") res |= MathT
    if (math) res |= CodeUnitT
    res

  // named record lookup
  private def lookupName(obj: Set[String], prop: ValueTy): ValueTy =
    var res = ValueTy()
    val str = prop.str
    for {
      name <- obj
      propStr <- str match
        case Inf      => exploded(s"too imprecise field name: $name[⊤]")
        case Fin(set) => set
    } res |= cfg.tyModel.getProp(name, propStr)
    res

  // record lookup
  private def lookupRecord(record: RecordTy, prop: ValueTy): ValueTy =
    val str = prop.str
    var res = ValueTy()
    def add(propStr: String): Unit = record.map.get(propStr) match
      case None =>
        logger.warn(s"invalid record property access: $record[$propStr]")
      case Some(None) =>
        exploded(s"too imprecise field access: $record[$propStr]")
      case Some(Some(ty)) =>
        res |= ty
    if (!record.isBottom) str match
      case Inf =>
        exploded(s"too imprecise field name: $record[⊤]")
      case Fin(set) =>
        for (propStr <- set) add(propStr)
    res

  // list lookup
  private def lookupList(list: ListTy, prop: ValueTy): ValueTy =
    var res = ValueTy()
    val str = prop.str
    val math = prop.math
    for (ty <- list.elem)
      if (str contains "length") res |= MathT
      if (math) res |= ty
    res

  // symbol lookup
  private def lookupSymbol(symbol: Boolean, prop: ValueTy): ValueTy =
    if (symbol && prop.str.contains("Description")) StrTopT
    else ValueTy()

  // submap lookup
  private def lookupSubMap(subMap: SubMapTy, prop: ValueTy): ValueTy =
    if (!subMap.isBottom) ValueTy(pureValue = subMap.value)
    else ValueTy()
}
