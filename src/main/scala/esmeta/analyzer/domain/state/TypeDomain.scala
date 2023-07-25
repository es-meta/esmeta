package esmeta.analyzer.domain.state

import esmeta.LINE_SEP
import esmeta.analyzer.*
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
    def get(base: AbsValue, prop: AbsValue, ref: Option[Ref]): AbsValue =
      val baseTy = base.ty
      val propTy = prop.ty
      AbsValue(
        lookupComp(baseTy.comp, propTy, ref) ||
        lookupAst(baseTy.astValue, propTy, ref) ||
        lookupStr(baseTy.str, propTy, ref, baseTy) ||
        lookupList(baseTy.list, propTy) ||
        lookupName(baseTy.name, propTy, ref, baseTy) ||
        lookupRecord(baseTy.record, propTy) ||
        lookupSymbol(baseTy.symbol, propTy) ||
        lookupSubMap(baseTy.subMap, propTy),
      )

    /** getters with an address partition */
    def get(part: Part): AbsObj = AbsObj.Bot

    /** lookup global variables */
    def lookupGlobal(x: Global): AbsValue = base.getOrElse(x, AbsValue.Bot)

    /** identifier setter */
    def update(x: Id, value: AbsValue): Elem = x match
      case x: Local  => defineLocal(x -> value)
      case x: Global =>
        // TODO if (value !⊑ base(x))
        //   warning(s"invalid global variable update: $x = $value")
        elem

    /** property setter */
    def update(base: AbsValue, prop: AbsValue, value: AbsValue): Elem = elem

    /** deletion with reference values */
    def delete(refV: AbsRefValue): Elem = elem

    /** push values to a list */
    def push(list: AbsValue, value: AbsValue, front: Boolean): Elem = elem

    /** remove a value in a list */
    def remove(list: AbsValue, value: AbsValue): Elem = elem

    /** pop a value in a list */
    def pop(list: AbsValue, front: Boolean): (AbsValue, Elem) =
      (list.ty.list.elem.fold(AbsValue.Bot)(AbsValue(_)), elem)

    /** set a type to an address partition */
    def setType(v: AbsValue, tname: String): (AbsValue, Elem) =
      (AbsValue(NameT(tname)), elem)

    /** copy object */
    def copyObj(to: AllocSite, from: AbsValue): (AbsValue, Elem) = (from, elem)

    /** get object keys */
    def keys(
      to: AllocSite,
      v: AbsValue,
      intSorted: Boolean,
    ): (AbsValue, Elem) =
      val value =
        if (v.ty.subMap.isBottom) AbsValue.Bot
        else AbsValue(StrT)
      (value, elem)

    /** list concatenation */
    def concat(
      to: AllocSite,
      lists: Iterable[AbsValue] = Nil,
    ): (AbsValue, Elem) =
      val value = AbsValue(ListT((for {
        list <- lists
        elem <- list.ty.list.elem
      } yield elem).foldLeft(ValueTy())(_ || _)))
      (value, elem)

    /** get childeren of AST */
    def getChildren(
      to: AllocSite,
      ast: AbsValue,
    ): (AbsValue, Elem) = (AbsValue(ListT(AstT)), elem)

    /** get items of AST */
    def getItems(
      to: AllocSite,
      nt: AbsValue,
      ast: AbsValue,
    ): (AbsValue, Elem) = nt.ty.nt.getSingle match
      case One(Nt(name, _)) => (AbsValue(ListT(AstT(name))), elem)
      case Many             => exploded(s"imprecise nt name: $nt")
      case Zero             => (AbsValue.Bot, Bot)

    /** allocation of map with address partitions */
    def allocMap(
      to: AllocSite,
      tname: String,
      pairs: Iterable[(AbsValue, AbsValue)],
    ): (AbsValue, Elem) =
      val value =
        if (tname == "Record") RecordT((for {
          (k, v) <- pairs
        } yield k.getSingle match
          case One(Str(key)) => key -> v.ty
          case _             => exploded(s"imprecise field name: $k")
        ).toMap)
        else NameT(tname)
      (AbsValue(value), elem)

    /** allocation of list with address partitions */
    def allocList(
      to: AllocSite,
      list: Iterable[AbsValue] = Nil,
    ): (AbsValue, Elem) =
      val listT = ListT(list.foldLeft(ValueTy()) { case (l, r) => l || r.ty })
      (AbsValue(listT), elem)

    /** allocation of symbol with address partitions */
    def allocSymbol(to: AllocSite, desc: AbsValue): (AbsValue, Elem) =
      (AbsValue(SymbolT), elem)

    /** check contains */
    def contains(
      list: AbsValue,
      value: AbsValue,
      field: Option[(Type, String)],
    ): AbsValue =
      if (list.ty.list.isBottom) AbsValue.Bot
      else AbsValue.boolTop

    /** define global variables */
    def defineGlobal(pairs: (Global, AbsValue)*): Elem = elem

    /** define local variables */
    def defineLocal(pairs: (Local, AbsValue)*): Elem =
      elem.copy(locals = locals ++ pairs)

    /** singleton checks */
    override def isSingle: Boolean = false

    /** singleton address partition checks */
    def isSingle(part: Part): Boolean = false

    /** find merged parts */
    def findMerged: Unit = {}

    /** handle calls */
    def doCall: Elem = elem
    def doProcStart(fixed: Set[Part]): Elem = elem

    /** handle returns (elem: return states / to: caller states) */
    def doReturn(
      to: Elem,
      defs: Iterable[(Local, AbsValue)],
    ): Elem = Elem(
      reachable = true,
      locals = to.locals ++ defs,
    )

    def doProcEnd(to: Elem, defs: (Local, AbsValue)*): Elem = elem
    def doProcEnd(to: Elem, defs: Iterable[(Local, AbsValue)]): Elem = elem

    /** garbage collection */
    def garbageCollected: Elem = elem

    /** get reachable address partitions */
    def reachableParts: Set[Part] = Set()

    /** copy */
    def copied(locals: Map[Local, AbsValue] = Map()): Elem =
      elem.copy(locals = locals)

    /** get string */
    def getString(detail: Boolean): String = elem.toString

    /** get string with detailed shapes of locations */
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
    CONSTT_BREAK || CONSTT_CONTINUE || CONSTT_RETURN || CONSTT_THROW
  private def lookupComp(
    comp: CompTy,
    prop: ValueTy,
    ref: Option[Ref],
  ): ValueTy =
    val str = prop.str
    val normal = !comp.normal.isBottom
    val abrupt = !comp.abrupt.isBottom
    var res = ValueTy()
    if (str contains "Value")
      if (normal) res ||= ValueTy(pureValue = comp.normal)
      if (abrupt) {
        if (comp.abrupt.contains("return") || comp.abrupt.contains("throw"))
          res ||= ESValueT
        if (comp.abrupt.contains("continue") || comp.abrupt.contains("break"))
          res || CONSTT_EMPTY
      }
    if (str contains "Target")
      if (normal) res ||= CONSTT_EMPTY
      if (abrupt) res ||= StrT || CONSTT_EMPTY
    if (str contains "Type")
      if (normal) res ||= CONSTT_NORMAL
      if (abrupt) res ||= constTyForAbruptTarget
    if (!comp.isBottom)
      boundCheck(
        prop,
        StrT("Value", "Target", "Type"),
        (cp, t) =>
          val plp = PropertyLookupPoint(LookupKind.Str, cp, ref)
          UnknownPropertyMismatch(plp, comp, t),
      )
    res

  // AST lookup
  private def lookupAst(
    ast: AstValueTy,
    prop: ValueTy,
    ref: Option[Ref],
  ): ValueTy =
    if (!ast.isBottom)
      boundCheck(
        prop,
        MathT || StrT,
        (cp, t) =>
          val plp = PropertyLookupPoint(LookupKind.Ast, cp, ref)
          UnknownPropertyMismatch(plp, ast, t),
      )
    ast match
      case AstValueTy.Bot => ValueTy.Bot
      case AstSingleTy(name, idx, subIdx) =>
        lookupAstIdxProp(name, idx, subIdx)(prop) ||
        lookupAstStrProp(prop)
      case AstNameTy(names) =>
        if (!prop.math.isBottom) AstT // TODO more precise
        else lookupAstStrProp(prop)
      case _ => AstT

  // lookup index properties of ASTs
  private def lookupAstIdxProp(
    name: String,
    idx: Int,
    subIdx: Int,
  )(prop: ValueTy): ValueTy = prop.math.getSingle match
    case One(n) if n.isValidInt =>
      val propIdx = n.toInt
      val rhs = cfg.grammar.nameMap(name).rhsList(idx)
      val nts = rhs.getNts(subIdx)
      nts(propIdx).fold(AbsentT)(AstT(_))
    case Zero | One(_) => ValueTy.Bot
    case _             => AstT // TODO more precise

  // lookup string properties of ASTs
  private def lookupAstStrProp(prop: ValueTy): ValueTy =
    val nameMap = cfg.grammar.nameMap
    prop.str.getSingle match
      case Zero                               => ValueTy.Bot
      case One(name) if nameMap contains name => AstT(name)
      case _ => AstT // TODO warning(s"invalid access: $name of $ast")

  // string lookup
  private def lookupStr(
    str: BSet[String],
    prop: ValueTy,
    ref: Option[Ref],
    baseTy: ValueTy,
  ): ValueTy =
    if (str.isBottom) ValueTy.Bot
    else {
      var res = ValueTy.Bot
      if (prop.str contains "length") res ||= MathT
      if (!prop.math.isBottom) res ||= CodeUnitT
      // invalid access on string type
      boundCheck(
        prop,
        MathT || StrT("length"),
        (cp, t) =>
          val plp = PropertyLookupPoint(LookupKind.Str, cp, ref)
          UnknownPropertyMismatch(plp, baseTy, t),
      )
      res
    }

  // named record lookup
  private def lookupName(
    obj: NameTy,
    prop: ValueTy,
    ref: Option[Ref],
    baseTy: ValueTy,
  ): ValueTy =
    var res = ValueTy()
    val str = prop.str
    for {
      name <- obj.set
      propStr <- str match
        case Inf =>
          if (name == "IntrinsicsRecord") res ||= ObjectT
          Set()
        case Fin(set) => set
    } {
      res ||= cfg.tyModel.getPropOrElse(name, propStr) {
        analyzer match {
          case ta: TypeAnalyzer => {
            if (ta.config.unknownProperty)
              for { cp <- sem.curCp } {
                val plp = PropertyLookupPoint(LookupKind.Name, cp, ref)
                ta.addMismatch(UnknownPropertyMismatch(plp, baseTy, prop))
              }
          }
        }
        AbsentT
      }
    }
    res

  // record lookup
  private def lookupRecord(record: RecordTy, prop: ValueTy): ValueTy =
    val str = prop.str
    var res = ValueTy()
    def add(propStr: String): Unit = record match
      case RecordTy.Top       =>
      case RecordTy.Elem(map) => map.get(propStr).map(res ||= _)
    if (!record.isBottom) str match
      case Inf =>
      case Fin(set) =>
        for (propStr <- set) add(propStr)
    res

  // list lookup
  private def lookupList(list: ListTy, prop: ValueTy): ValueTy =
    var res = ValueTy()
    val str = prop.str
    val math = prop.math
    for (ty <- list.elem)
      if (str contains "length") res ||= MathT
      if (!math.isBottom) res ||= ty
    res

  // symbol lookup
  private def lookupSymbol(symbol: Boolean, prop: ValueTy): ValueTy =
    if (symbol && prop.str.contains("Description")) StrT
    else ValueTy()

  // submap lookup
  private def lookupSubMap(subMap: SubMapTy, prop: ValueTy): ValueTy =
    if (!subMap.isBottom) ValueTy(pureValue = subMap.value)
    else ValueTy()

  // bound check
  private def boundCheck(
    ty: ValueTy,
    boundTy: => ValueTy,
    f: (ControlPoint, ValueTy) => TypeMismatch,
  ): Unit =
    val other = ty -- boundTy
    if (!other.isBottom)
      analyzer match {
        case ta: TypeAnalyzer => {
          if (ta.config.unknownProperty)
            for { cp <- sem.curCp } {
              ta.addMismatch(f(cp, other))
            }
        }
      }
}
