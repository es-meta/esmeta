package esmeta.analyzer.domain.state

import esmeta.LINE_SEP
import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*
import esmeta.ir.{*, given}
import esmeta.es
import esmeta.es.*
import esmeta.es.builtin.*
import esmeta.ty.*
import esmeta.ty.util.Stringifier.{*, given}
import esmeta.util.*
import esmeta.util.Appender
import esmeta.util.Appender.{*, given}
import esmeta.util.BaseUtils.{optional => opt, *}

trait StateTypeDomainDecl { self: Self =>

  /** type domain for states */
  class StateTypeDomain(analyzer: TypeAnalyzer) extends StateDomain {

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
            elem.get(x) ⊑ that.get(x)
          })

      /** join operator */
      def ⊔(that: Elem): Elem = (elem, that) match
        case _ if elem.isBottom => that
        case _ if that.isBottom => elem
        case (l, r) =>
          val newLocals = (for {
            x <- (l.locals.keySet ++ r.locals.keySet).toList
            v = elem.get(x) ⊔ that.get(x)
          } yield x -> v).toMap
          Elem(true, newLocals)

      /** meet operator */
      override def ⊓(that: Elem): Elem = (elem, that) match
        case _ if elem.isBottom || that.isBottom => Bot
        case (l, r) =>
          val newLocals = (for {
            x <- (l.locals.keySet ++ r.locals.keySet).toList
            v = elem.get(x) ⊓ that.get(x)
          } yield x -> v).toMap
          Elem(true, newLocals)

      // -----------------------------------------------------------------------
      // Operations for Abstract States
      // -----------------------------------------------------------------------

      /** getter */
      def get(x: Var): AbsValue = x match
        case x: Global => base.getOrElse(x, AbsValue.Bot)
        case x: Local  => elem.locals.getOrElse(x, AbsValue.Bot)

      /** getter */
      def get(base: AbsValue, field: AbsValue): AbsValue =
        val baseTy = base.ty
        val fieldTy = field.ty
        AbsValue(
          lookupAst(baseTy.ast, fieldTy) ||
          lookupStr(baseTy.str, fieldTy) ||
          lookupList(baseTy.list, fieldTy) ||
          lookupRecord(baseTy.record, fieldTy) ||
          lookupMap(baseTy.map, fieldTy),
        )

      /** getter */
      def get(part: Part): AbsObj = error("do not support address partitions")

      /** define variables */
      def define(x: Var, value: AbsValue): Elem = x match
        case x: Local  => elem.copy(locals = locals + (x -> value))
        case x: Global => error("do not support defining global variables")

      /** identifier setter */
      def update(x: Var, value: AbsValue): Elem = x match
        case x: Local  => elem.copy(locals = locals + (x -> value))
        case x: Global => elem

      /** field setter */
      def update(base: AbsValue, field: AbsValue, value: AbsValue): Elem = elem

      /** variable existence check */
      def exists(x: Var): AbsValue = AbsValue(x match
        case x: Global => base.contains(x)
        case x: Local  => elem.locals.contains(x),
      )

      /** field existence check */
      def exists(base: AbsValue, field: AbsValue): AbsValue = ???

      /** expand a field of a record object */
      def expand(base: AbsValue, field: AbsValue): Elem = elem

      /** delete a key from an map object */
      def delete(base: AbsValue, field: AbsValue): Elem = elem

      /** push a value to a list */
      def push(list: AbsValue, value: AbsValue, front: Boolean): Elem = elem

      /** pop a value from a list */
      def pop(list: AbsValue, front: Boolean): (AbsValue, Elem) = ???
      // (list.ty.list.elem.fold(AbsValue.Bot)(AbsValue(_)), elem)

      /** copy object */
      def copy(from: AbsValue)(asite: AllocSite): (AbsValue, Elem) =
        (from, elem)

      /** get keys of a record/map object as a list */
      def keys(
        obj: AbsValue,
        intSorted: Boolean,
      )(asite: AllocSite): (AbsValue, Elem) =
        val value =
          if (obj.ty.map.isBottom) AbsValue.Bot
          else AbsValue(ListT(StrT))
        (value, elem)

      // ------------------------------ TODO ------------------------------

      /** list concatenation */
      def concat(
        to: AllocSite,
        lists: Iterable[AbsValue] = Nil,
      ): (AbsValue, Elem) = ???
      // val value = AbsValue(ListT((for {
      //   list <- lists
      //   elem <- list.ty.list.elem
      // } yield elem).foldLeft(BotT)(_ || _)))
      // (value, elem)

      /** get childeren of AST */
      def getChildren(
        to: AllocSite,
        ast: AbsValue,
      ): (AbsValue, Elem) = (AbsValue(ListT(AstT)), elem)

      /** get items of AST */
      def getItems(
        to: AllocSite,
        grammarSymbol: AbsValue,
        ast: AbsValue,
      ): (AbsValue, Elem) = grammarSymbol.ty.grammarSymbol.getSingle match
        case One(GrammarSymbol(name, _)) => (AbsValue(ListT(AstT(name))), elem)
        case Many => exploded(s"imprecise grammarSymbol name: $grammarSymbol")
        case Zero => (AbsValue.Bot, Bot)

      /** allocation of map with address partitions */
      def allocMap(
        to: AllocSite,
        pairs: Iterable[(AbsValue, AbsValue)],
      ): (AbsValue, Elem) =
        val (keys, values) = pairs.unzip
        val key = keys.foldLeft(BotT)(_ || _.ty)
        val value = values.foldLeft(BotT)(_ || _.ty)
        (AbsValue(MapT(key, value)), elem)

      /** allocation of record with address partitions */
      def allocRecord(
        to: AllocSite,
        tname: String,
        pairs: Iterable[(String, AbsValue)],
      ): (AbsValue, Elem) =
        // val value = tnameOpt match
        //   case None => RecordT(pairs.map { case (f, v) => f -> v.ty }.toMap)
        //   case Some(tname) => NameT(tname)
        // (AbsValue(value), elem)
        ???

      /** allocation of list with address partitions */
      def allocList(
        to: AllocSite,
        list: Iterable[AbsValue] = Nil,
      ): (AbsValue, Elem) =
        val listT = ListT(list.foldLeft(BotT) { case (l, r) => l || r.ty })
        (AbsValue(listT), elem)

      /** check contains */
      def contains(list: AbsValue, value: AbsValue): AbsValue =
        if (list.ty.list.isBottom) AbsValue.Bot
        else AbsValue.boolTop

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
        app >> elem.locals
      } else app >> "⊥"

    // completion record lookup
    lazy val enumTyForAbruptTarget =
      ENUMT_BREAK || ENUMT_CONTINUE || ENUMT_RETURN || ENUMT_THROW

    // AST lookup
    private def lookupAst(ast: AstTy, field: ValueTy): ValueTy =
      import AstTy.*
      ast match
        case AstTy.Bot => BotT
        case Detail(name, idx) =>
          lookupAstIdxField(name, idx)(field) ||
          lookupAstStrField(field)
        case Simple(names) =>
          if (!field.math.isBottom) AstT // TODO more precise
          else lookupAstStrField(field)
        case _ => AstT

    // lookup index fields of ASTs
    private def lookupAstIdxField(
      name: String,
      idx: Int,
    )(field: ValueTy): ValueTy = field.math.getSingle match
      case Zero => BotT
      case _    => AstT // TODO more precise

    // lookup string fields of ASTs
    private def lookupAstStrField(field: ValueTy): ValueTy =
      val nameMap = cfg.grammar.nameMap
      field.str.getSingle match
        case Zero                               => BotT
        case One(name) if nameMap contains name => AstT(name)
        case _ => AstT // TODO warning(s"invalid access: $name of $ast")

    // string lookup
    private def lookupStr(str: BSet[String], field: ValueTy): ValueTy =
      if (str.isBottom) BotT
      else {
        var res = BotT
        if (field.str contains "length") res ||= NonNegIntT
        if (!field.math.isBottom) res ||= CodeUnitT
        res
      }

    // record lookup
    private def lookupRecord(record: RecordTy, field: ValueTy): ValueTy =
      val str = field.str
      var res = BotT
      def add(fieldStr: String): Unit = res ||= record(fieldStr).value
      if (!record.isBottom) for (fieldStr <- str) add(fieldStr)
      res

    // list lookup
    private def lookupList(list: ListTy, field: ValueTy): ValueTy =
      var res = BotT
      val str = field.str
      val math = field.math
      list match
        case ListTy.Top        => AnyT
        case ListTy.Elem(elem) => elem
        case ListTy.Bot        => BotT

    // symbol lookup
    private def lookupSymbol(symbol: Boolean, field: ValueTy): ValueTy =
      if (symbol && field.str.contains("Description")) StrT
      else BotT

    // map lookup
    private def lookupMap(map: MapTy, field: ValueTy): ValueTy = map match
      case MapTy.Top              => AnyT
      case MapTy.Elem(key, value) => value
      case MapTy.Bot              => BotT

    // bound check
    private def boundCheck(
      ty: ValueTy,
      boundTy: => ValueTy,
      f: ValueTy => String,
    ): Unit =
      // val other = ty -- boundTy
      // if (!other.isBottom) warning(f(other))
      ()
  }
}
