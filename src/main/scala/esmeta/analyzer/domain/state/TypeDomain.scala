package esmeta.analyzer.domain.state

import esmeta.LINE_SEP
import esmeta.analyzer.*
import esmeta.analyzer.Config.*
import esmeta.analyzer.domain.*
import esmeta.state.*
import esmeta.ir.*
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
  private var base: Map[Id, AbsValue] = Map()

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
      val propStr = propTy.str
      var res = ValueTy()
      if (propStr contains "Value") res |= ValueTy(normal = baseTy.normal)
      ???
    // val vset = for {
    //   ty <- base.set
    //   p <- prop.set
    //   v <- ty match
    //     case comp: CompType  => lookupComp(comp, p, check)
    //     case AstTopT         => lookupAst(p, check)
    //     case ast: AstTBase   => lookupAst(ast, p, check)
    //     case StrT            => lookupStr(p, check)
    //     case str: StrSingleT => lookupStr(str, p, check)
    //     case list: ListT     => lookupList(list, p, check)
    //     case NilT            => lookupList(p, check)
    //     case obj: NameT      => lookupNamedRec(obj, p, check)
    //     case MapT(elemTy)    => Set(elemTy, AbsentT)
    //     case rec: RecordT    => lookupRec(rec, p, check)
    //     case SymbolT         => lookupSymbol(p, check)
    //     case _ =>
    //       if (check) warning(s"invalid property access: $ty[$p]")
    //       Set()
    // } yield v
    // AbsValue(vset.toList: _*)

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

  // appender generator
  private def mkRule(detail: Boolean): Rule[Elem] = (app, elem) =>
    val irStringifier = IRElem.getStringifier(detail, false)
    import irStringifier.given
    if (!elem.isBottom) app.wrap {
      app >> elem.locals >> LINE_SEP
    }
    else app >> "⊥"
}
