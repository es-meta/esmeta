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
      def exists(x: Var): AbsValue = AbsValue.boolTop

      /** field existence check */
      def exists(base: AbsValue, field: AbsValue): AbsValue = AbsValue(BoolT)

      /** expand a field of a record object */
      def expand(base: AbsValue, field: AbsValue): Elem = elem

      /** delete a key from an map object */
      def delete(base: AbsValue, field: AbsValue): Elem = elem

      /** push a value to a list */
      def push(list: AbsValue, value: AbsValue, front: Boolean): Elem = elem

      /** pop a value from a list */
      def pop(list: AbsValue, front: Boolean): (AbsValue, Elem) =
        (AbsValue(list.ty.list.elem), elem)

      /** copy object */
      def copy(from: AbsValue)(asite: AllocSite): (AbsValue, Elem) =
        (from, elem)

      /** get keys of a record/map object as a list */
      def keys(
        base: AbsValue,
        intSorted: Boolean,
      )(asite: AllocSite): (AbsValue, Elem) =
        val ty = base.ty
        var elemTy = BotT
        if (!ty.record.isBottom) elemTy ||= ty.record.getKey
        if (!ty.map.isBottom) elemTy ||= ty.map.getKey
        if (elemTy.isBottom) (AbsValue.Bot, Bot)
        else (AbsValue(ListT(elemTy)), elem)

      /** allocate a record object */
      def allocRecord(
        tname: String,
        pairs: Iterable[(String, AbsValue)] = Nil,
      )(asite: AllocSite): (AbsValue, Elem) =
        (AbsValue(RecordT(tname, pairs.map(_ -> _.ty).toMap)), elem)

      /** allocate a map object */
      def allocMap(
        pairs: Iterable[(AbsValue, AbsValue)] = Nil,
      )(asite: AllocSite): (AbsValue, Elem) =
        val (keys, values) = pairs.unzip
        val key = keys.foldLeft(BotT)(_ || _.ty)
        val value = values.foldLeft(BotT)(_ || _.ty)
        (AbsValue(MapT(key, value)), elem)

      /** allocate a list object */
      def allocList(
        vs: Iterable[AbsValue] = Nil,
      )(asite: AllocSite): (AbsValue, Elem) =
        (AbsValue(ListT(vs.foldLeft(BotT)(_ || _.ty))), elem)

      /** check contains */
      def contains(list: AbsValue, value: AbsValue): AbsValue =
        if (list.ty.list.isBottom) AbsValue.Bot
        else AbsValue.boolTop

      /** handle returns (elem: return states / to: caller states) */
      def doReturn(to: Elem, lhs: Local, value: AbsValue): Elem = Elem(
        reachable = true,
        locals = to.locals + (lhs -> value),
      )

      /** singleton address partition checks */
      def isSingle(part: Part): Boolean = false

      /** set local environments */
      def setLocal(locals: Map[Local, AbsValue]): Elem =
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

      // -----------------------------------------------------------------------
      // Helpers for Debugging
      // -----------------------------------------------------------------------

      /** find merged parts */
      def findMerged: Unit = {}

      /** get reachable address partitions */
      def reachableParts: Set[Part] = Set()

      /** has top elements */
      def hasTop: Boolean = locals.values.exists(_.ty.isTop)
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
  }
}
