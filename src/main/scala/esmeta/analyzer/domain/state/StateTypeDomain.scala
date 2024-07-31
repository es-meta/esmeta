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
    given rule: Rule[Elem] = ??? // mkRule(true)

    /** simpler appender */
    private val shortRule: Rule[Elem] = ??? // mkRule(false)

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

      /** getters with bases and fields */
      def get(base: AbsValue, field: AbsValue): AbsValue =
        // val baseTy = base.ty
        // val fieldTy = field.ty
        // AbsValue(
        //   lookupComp(baseTy.comp, fieldTy) ||
        //   lookupAst(baseTy.astValue, fieldTy) ||
        //   lookupStr(baseTy.str, fieldTy) ||
        //   lookupList(baseTy.list, fieldTy) ||
        //   lookupName(baseTy.name, fieldTy) ||
        //   lookupRecord(baseTy.record, fieldTy) ||
        //   lookupMap(baseTy.map, fieldTy),
        // )
        ???

      /** getters with an address partition */
      def get(part: Part): AbsObj = AbsObj.Bot

      /** lookup global variables */
      def lookupGlobal(x: Global): AbsValue = base.getOrElse(x, AbsValue.Bot)

      /** identifier setter */
      def update(x: Var, value: AbsValue): Elem = x match
        case x: Local  => defineLocal(x -> value)
        case x: Global =>
          // TODO if (value !⊑ base(x))
          //   warning(s"invalid global variable update: $x = $value")
          elem

      /** field setter */
      def update(base: AbsValue, field: AbsValue, value: AbsValue): Elem = elem

      /** deletion with reference values */
      def delete(rt: AbsRefTarget): Elem = elem

      /** push values to a list */
      def push(list: AbsValue, value: AbsValue, front: Boolean): Elem = elem

      /** pop a value in a list */
      def pop(list: AbsValue, front: Boolean): (AbsValue, Elem) = ???
      // (list.ty.list.elem.fold(AbsValue.Bot)(AbsValue(_)), elem)

      /** copy object */
      def copyObj(to: AllocSite, from: AbsValue): (AbsValue, Elem) =
        (from, elem)

      /** get object keys */
      def keys(
        to: AllocSite,
        v: AbsValue,
        intSorted: Boolean,
      ): (AbsValue, Elem) =
        val value =
          if (v.ty.map.isBottom) AbsValue.Bot
          else AbsValue(ListT(StrT))
        (value, elem)

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

    // // appender generator
    // private def mkRule(detail: Boolean): Rule[Elem] = (app, elem) =>
    //   if (!elem.isBottom) {
    //     val irStringifier = IRElem.getStringifier(detail, false)
    //     import irStringifier.given
    //     given Rule[Map[Local, AbsValue]] = sortedMapRule(sep = ": ")
    //     app >> elem.locals
    //   } else app >> "⊥"

    // // completion record lookup
    // lazy val enumTyForAbruptTarget =
    //   ENUMT_BREAK || ENUMT_CONTINUE || ENUMT_RETURN || ENUMT_THROW
    // private def lookupComp(comp: CompTy, field: ValueTy): ValueTy =
    //   val str = field.str
    //   val normal = !comp.normal.isBottom
    //   val abrupt = !comp.abrupt.isBottom
    //   var res = BotT
    //   if (str contains "Value")
    //     if (normal) res ||= ValueTy(pureValue = comp.normal)
    //     if (abrupt) {
    //       if (comp.abrupt.contains("return") || comp.abrupt.contains("throw"))
    //         res ||= ESValueT
    //       if (comp.abrupt.contains("continue") || comp.abrupt.contains("break"))
    //         res || ENUMT_EMPTY
    //     }
    //   if (str contains "Target")
    //     if (normal) res ||= ENUMT_EMPTY
    //     if (abrupt) res ||= StrT || ENUMT_EMPTY
    //   if (str contains "Type")
    //     if (normal) res ||= ENUMT_NORMAL
    //     if (abrupt) res ||= enumTyForAbruptTarget
    //   // TODO if (!comp.isBottom)
    //   //   boundCheck(
    //   //     field,
    //   //     StrT("Value", "Target", "Type"),
    //   //     t => s"invalid access: $t of $comp",
    //   //   )
    //   res

    // // AST lookup
    // private def lookupAst(ast: AstValueTy, field: ValueTy): ValueTy = ast match
    //   case AstValueTy.Bot => BotT
    //   case AstSingleTy(name, idx, subIdx) =>
    //     lookupAstIdxField(name, idx, subIdx)(field) ||
    //     lookupAstStrField(field)
    //   case AstNameTy(names) =>
    //     if (!field.math.isBottom) AstT // TODO more precise
    //     else lookupAstStrField(field)
    //   case _ => AstT
    // // TODO if (!ast.isBottom)
    // //   boundCheck(field, MathT || StrT, t => s"invalid access: $t of $ast")

    // // lookup index fields of ASTs
    // private def lookupAstIdxField(
    //   name: String,
    //   idx: Int,
    //   subIdx: Int,
    // )(field: ValueTy): ValueTy = field.math.getSingle match
    //   case One(Math(n)) if n.isValidInt =>
    //     val fieldIdx = n.toInt
    //     val rhs = cfg.grammar.nameMap(name).rhsList(idx)
    //     val nts = rhs.getGrammarSymbols(subIdx)
    //     nts(fieldIdx).fold(UninitT)(AstT(_))
    //   case Zero | One(_) => BotT
    //   case _             => AstT // TODO more precise

    // // lookup string fields of ASTs
    // private def lookupAstStrField(field: ValueTy): ValueTy =
    //   val nameMap = cfg.grammar.nameMap
    //   field.str.getSingle match
    //     case Zero                               => BotT
    //     case One(name) if nameMap contains name => AstT(name)
    //     case _ => AstT // TODO warning(s"invalid access: $name of $ast")

    // // string lookup
    // private def lookupStr(str: BSet[String], field: ValueTy): ValueTy =
    //   if (str.isBottom) BotT
    //   else {
    //     var res = BotT
    //     if (field.str contains "length") res ||= NonNegIntT
    //     if (!field.math.isBottom) res ||= CodeUnitT
    //     // TODO if (!str.isBottom)
    //     //   boundCheck(
    //     //     field,
    //     //     MathT || StrT("length"),
    //     //     t => s"invalid access: $t of ${PureValueTy(str = str)}",
    //     //   )
    //     res
    //   }

    // // named record lookup
    // private val INTRINSICS_NAME_TY = NameTy("Intrinsics")
    // private def lookupName(obj: NameTy, field: ValueTy): ValueTy =
    //   if (obj == INTRINSICS_NAME_TY) lookupIntrinsics(field)
    //   else
    //     (for {
    //       name <- obj.set
    //       fieldStr <- field.str
    //     } yield cfg.tyModel.getField(name, fieldStr)).foldLeft(BotT)(_ || _)

    // // intrinsics lookup
    // private def lookupIntrinsics(field: ValueTy): ValueTy = field.str match
    //   case Inf => ObjectT
    //   case Fin(set) =>
    //     NameT(for {
    //       s <- set
    //       if s.startsWith("%") && s.endsWith("%")
    //       fieldStr = s.substring(1, s.length - 1)
    //       addr = intrAddr(fieldStr)
    //       obj = opt(analyzer.init.initHeap(addr))
    //       tname <- obj match
    //         case Some(RecordObj(tname, _)) => Some(tname)
    //         case _                         => None
    //     } yield tname)

    // // record lookup
    // private def lookupRecord(record: RecordTy, field: ValueTy): ValueTy =
    //   val str = field.str
    //   var res = BotT
    //   def add(fieldStr: String): Unit = record match
    //     case RecordTy.Top       =>
    //     case RecordTy.Elem(map) => map.get(fieldStr).map(res ||= _)
    //   if (!record.isBottom) for (fieldStr <- str) add(fieldStr)
    //   res

    // // list lookup
    // private def lookupList(list: ListTy, field: ValueTy): ValueTy =
    //   var res = BotT
    //   val str = field.str
    //   val math = field.math
    //   for (ty <- list.elem)
    //     if (str contains "length") res ||= NonNegIntT
    //     if (!math.isBottom) res ||= ty
    //   res

    // // symbol lookup
    // private def lookupSymbol(symbol: Boolean, field: ValueTy): ValueTy =
    //   if (symbol && field.str.contains("Description")) StrT
    //   else BotT

    // // map lookup
    // private def lookupMap(map: MapTy, field: ValueTy): ValueTy =
    //   if (!map.isBottom) ValueTy(pureValue = map.value)
    //   else BotT

    // // bound check
    // private def boundCheck(
    //   ty: ValueTy,
    //   boundTy: => ValueTy,
    //   f: ValueTy => String,
    // ): Unit =
    //   // val other = ty -- boundTy
    //   // if (!other.isBottom) warning(f(other))
    //   ()
  }
}
