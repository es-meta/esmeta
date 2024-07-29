package esmeta.analyzer.domain.state

import esmeta.LINE_SEP
import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.cfg.CFG
import esmeta.ir.{*, given}
import esmeta.es
import esmeta.es.*
import esmeta.state.*
import esmeta.util.*
import esmeta.util.Appender
import esmeta.util.Appender.{*, given}
import esmeta.util.BaseUtils.*

trait StateBasicDomainDecl { self: Self =>

  /** basic domain for states */
  object StateBasicDomain extends StateDomain {

    /** elements */
    case class Elem(
      reachable: Boolean = false,
      locals: Map[Local, AbsValue] = Map(),
      globals: Map[Global, AbsValue] = Map(),
      heap: AbsHeap = AbsHeap.Bot,
    ) extends Appendable

    /** top element */
    lazy val Top: Elem = exploded("top abstract state")

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

    /** set bases */
    def setBase(init: Initialize): Unit =
      AbsHeap.setBase(init.initHeap)
      base = for ((x, v) <- init.initGlobal.toMap) yield x -> AbsValue(v)

    private var base: Map[Var, AbsValue] = Map()

    /** element interfaces */
    extension (elem: Elem) {

      /** bottom check */
      override def isBottom = !elem.reachable

      /** partial order */
      def ⊑(that: Elem): Boolean = (elem, that) match
        case _ if elem.isBottom => true
        case _ if that.isBottom => false
        case (
              Elem(_, llocals, lglobals, lheap),
              Elem(_, rlocals, rglobals, rheap),
            ) =>
          val localsB = (llocals.keySet ++ rlocals.keySet).forall(x => {
            elem.lookupLocal(x) ⊑ that.lookupLocal(x)
          })
          val globalsB = (lglobals.keySet ++ rglobals.keySet).forall(x => {
            elem.lookupGlobal(x) ⊑ that.lookupGlobal(x)
          })
          val heapB = lheap ⊑ rheap
          localsB && globalsB && heapB

      /** join operator */
      def ⊔(that: Elem): Elem = (elem, that) match
        case _ if elem.isBottom => that
        case _ if that.isBottom => elem
        case (l, r) =>
          val newLocals = (for {
            x <- (l.locals.keySet ++ r.locals.keySet).toList
            v = elem.lookupLocal(x) ⊔ that.lookupLocal(x)
          } yield x -> v).toMap
          val newGlobals = (for {
            x <- (l.globals.keySet ++ r.globals.keySet).toList
            v = elem.lookupGlobal(x) ⊔ that.lookupGlobal(x)
          } yield x -> v).toMap
          val newHeap = elem.heap ⊔ that.heap
          Elem(true, newLocals, newGlobals, newHeap)

      /** meet operator */
      override def ⊓(that: Elem): Elem = (elem, that) match
        case _ if elem.isBottom || that.isBottom => Bot
        case (l, r) =>
          var isBottom = false
          val newLocals = (for {
            x <- (l.locals.keySet ++ r.locals.keySet).toList
            v = elem.lookupLocal(x) ⊓ that.lookupLocal(x)
            _ = isBottom ||= v.isBottom
          } yield x -> v).toMap
          val newGlobals = (for {
            x <- (l.globals.keySet ++ r.globals.keySet).toList
            v = elem.lookupGlobal(x) ⊓ that.lookupGlobal(x)
            _ = isBottom ||= v.isBottom
          } yield x -> v).toMap
          val newHeap = elem.heap ⊓ that.heap
          if (newHeap.isBottom || isBottom) Bot
          else Elem(true, newLocals, newGlobals, newHeap)

      /** getters with bases and fields */
      def get(base: AbsValue, field: AbsValue): AbsValue =
        val compValue = AbsValue(pureValue = base.comp(field.str))
        val partValue = elem.heap(base.part, field)
        val astValue = lookupAst(base.astValue, field)
        val strValue = lookupStr(base.str, field)
        compValue ⊔ partValue ⊔ astValue ⊔ strValue

      /** getters with an address partition */
      def get(part: Part): AbsObj = elem.heap(part)

      /** lookup global variables */
      def lookupGlobal(x: Global): AbsValue =
        elem.globals.getOrElse(x, base.getOrElse(x, AbsValue.Bot))

      /** identifier setter */
      def update(x: Var, value: AbsValue): Elem =
        elem.bottomCheck(AbsValue)(value) {
          x match
            case x: Global if globals contains x =>
              elem.copy(globals = globals + (x -> value))
            case x: Name if locals contains x =>
              elem.copy(locals = locals + (x -> value))
            case x: Temp =>
              elem.copy(locals = locals + (x -> value))
            case _ => Bot
        }

      /** field setter */
      def update(base: AbsValue, field: AbsValue, value: AbsValue): Elem =
        elem.bottomCheck(AbsValue)(base, field, value) {
          elem.copy(heap = elem.heap.update(base.part, field, value))
        }

      /** deletion with reference values */
      def delete(refV: AbsRefTarget): Elem = refV match
        case AbsVarTarget(x) => error(s"cannot delete variable $x")
        case AbsFieldTarget(base, field) =>
          elem.bottomCheck(AbsValue)(base, field) {
            elem.copy(heap = elem.heap.delete(base.part, field))
          }

      /** push values to a list */
      def push(list: AbsValue, value: AbsValue, front: Boolean): Elem =
        elem.bottomCheck(AbsValue)(list, value) {
          if (front) elem.copy(heap = elem.heap.prepend(list.part, value))
          else elem.copy(heap = elem.heap.append(list.part, value))
        }

      /** pop a value in a list */
      def pop(list: AbsValue, front: Boolean): (AbsValue, Elem) =
        var v: AbsValue = AbsValue.Bot
        val st: Elem = elem.bottomCheck(AbsPart)(list.part) {
          val (newV, newH) = elem.heap.pop(list.part, front)
          v ⊔= newV
          elem.copy(heap = newH)
        }
        (v, st)

      /** set a type to an address partition */
      def setType(v: AbsValue, tname: String): (AbsValue, Elem) =
        elem.bottomCheck(AbsPart)(v.part) {
          (v, elem.copy(heap = elem.heap.setType(v.part, tname)))
        }

      /** copy object */
      def copyObj(to: AllocSite, from: AbsValue): (AbsValue, Elem) =
        val partV = AbsValue(to)
        elem.bottomCheck(AbsPart)(from.part) {
          (partV, elem.copy(heap = elem.heap.copyObj(from.part)(to)))
        }

      /** get object keys */
      def keys(
        to: AllocSite,
        v: AbsValue,
        intSorted: Boolean,
      ): (AbsValue, Elem) =
        val partV = AbsValue(to)
        elem.bottomCheck(AbsPart)(v.part) {
          (partV, elem.copy(heap = elem.heap.keys(v.part, intSorted)(to)))
        }

      /** list concatenation */
      def concat(
        to: AllocSite,
        lists: Iterable[AbsValue] = Nil,
      ): (AbsValue, Elem) =
        elem.bottomCheck(AbsValue)(lists) {
          import monad.*
          (for {
            partV <- id(_.allocList(to))
            part = partV.part
            _ <- join(for {
              list <- lists
            } yield modify(st => st.copy(heap = st.heap.concat(part, list))))
          } yield partV)(elem)
        }

      /** get childeren of AST */
      def getChildren(
        to: AllocSite,
        ast: AbsValue,
      ): (AbsValue, Elem) = ast.getSingle match
        case One(AstValue(syn: Syntactic)) =>
          val vs = syn.children.flatten.map(AbsValue(_))
          allocList(to, vs)
        case Many => exploded("EGetChildren")
        case _    => (AbsValue.Bot, Bot)

      /** get items of AST */
      def getItems(
        to: AllocSite,
        grammarSymbol: AbsValue,
        ast: AbsValue,
      ): (AbsValue, Elem) = (grammarSymbol.getSingle, ast.getSingle) match
        case (One(GrammarSymbol(name, _)), One(AstValue(ast))) =>
          val vs = ast.getItems(name).map(AbsValue(_))
          allocList(to, vs)
        case (Many, _) | (_, Many) => exploded("EGetItems")
        case _                     => (AbsValue.Bot, Bot)

      /** allocation of map with address partitions */
      def allocMap(
        to: AllocSite,
        pairs: Iterable[(AbsValue, AbsValue)],
      ): (AbsValue, Elem) =
        val partV = AbsValue(to)
        elem.bottomCheck(AbsValue)(
          pairs.flatMap { case (k, v) => List(k, v) },
        ) {
          (partV, elem.copy(heap = heap.allocMap(to, pairs)))
        }

      /** allocation of record with address partitions */
      def allocRecord(
        to: AllocSite,
        tnameOpt: Option[String],
        pairs: Iterable[(String, AbsValue)],
      ): (AbsValue, Elem) =
        val partV = AbsValue(to)
        (partV, elem.copy(heap = heap.allocRecord(to, tnameOpt, pairs)))

      /** allocation of list with address partitions */
      def allocList(
        to: AllocSite,
        list: Iterable[AbsValue] = Nil,
      ): (AbsValue, Elem) =
        val partV = AbsValue(to)
        elem.bottomCheck(AbsValue)(list) {
          (partV, elem.copy(heap = heap.allocList(to, list)))
        }

      /** check contains */
      def contains(list: AbsValue, value: AbsValue): AbsValue =
        heap.contains(list.part, value)

      /** define global variables */
      def defineGlobal(pairs: (Global, AbsValue)*): Elem =
        elem.bottomCheck(AbsValue)(pairs.unzip._2) {
          elem.copy(globals = elem.globals ++ pairs)
        }

      /** define local variables */
      def defineLocal(pairs: (Local, AbsValue)*): Elem =
        elem.bottomCheck(AbsValue)(pairs.unzip._2) {
          elem.copy(locals = elem.locals ++ pairs)
        }

      /** singleton checks */
      override def isSingle: Boolean =
        reachable &&
        locals.forall { case (_, v) => v.isSingle } &&
        globals.forall { case (_, v) => v.isSingle } &&
        heap.isSingle

      /** singleton address partition checks */
      def isSingle(part: Part): Boolean = elem.heap.isSingle(part)

      /** find merged parts */
      def findMerged: Unit = {
        // visited address partitions
        var visited = Set[Part]()

        // heap members
        val heap @ AbsHeap(map, merged) = elem.heap

        // auxiliary functions for values
        def auxValue(
          value: AbsValue,
          path: String,
          partPath: String = "",
        ): Unit =
          if (!value.isBottom && !value.isSingle)
            if (partPath == "") println(s"$path is merged: $value")
            else println(s"$path ($partPath) is merged: $value")
          for (part <- value.reachableParts) auxPart(part, path)

        // auxiliary functions for address partitions
        def auxPart(part: Part, path: String): Unit =
          if (!visited.contains(part) && map.contains(part))
            visited += part
            heap(part).findMerged(part, path, auxValue)

        for ((x, v) <- locals) auxValue(v, s"partal $x")
        for ((x, v) <- globals) auxValue(v, s"global $x")
        for (part <- map.keys if part.isNamed) auxPart(part, s"$part")
        for (part <- map.keys if !part.isNamed)
          auxPart(part, s"<unreachable>")
      }

      /** handle calls */
      def doCall: Elem = elem
        .copy(heap = heap.doCall)
        .garbageCollected
      def doProcStart(fixed: Set[Part]): Elem = elem
        .copy(heap = heap.doProcStart(fixed))
        .garbageCollected

      /** handle returns (elem: return states / to: caller states) */
      def doReturn(to: Elem, defs: Iterable[(Local, AbsValue)]): Elem = Elem(
        reachable = true,
        locals = to.locals ++ defs,
        globals = globals,
        heap = heap.doReturn(to.heap),
      ).garbageCollected
      def doProcEnd(to: Elem, defs: (Local, AbsValue)*): Elem =
        doProcEnd(to, defs)
      def doProcEnd(to: Elem, defs: Iterable[(Local, AbsValue)]): Elem = Elem(
        reachable = true,
        locals = to.locals ++ defs,
        globals = globals,
        heap = heap.doProcEnd(to.heap),
      ).garbageCollected

      // TODO garbage collection
      def garbageCollected: Elem = elem
      // if (USE_GC) {
      //   val unreachParts = (heap.map.keySet -- reachableParts).filter(!_.isNamed)
      //   copy(heap = heap.removeParts(unreachParts))
      // } else elem

      /** get reachable address partitions */
      def reachableParts: Set[Part] =
        var parts = Set[Part]()
        for ((_, v) <- locals) parts ++= v.reachableParts
        for ((_, v) <- globals) parts ++= v.reachableParts
        heap.reachableParts(parts)

      /** copy */
      def copied(
        locals: Map[Local, AbsValue] = Map(),
      ): Elem = elem.copy(locals = locals)

      /** get string */
      def getString(detail: Boolean): String =
        val app = Appender()
        given Rule[Elem] = if (detail) rule else shortRule
        app >> elem
        app.toString

      /** get string with detailed shapes of locations */
      def getString(value: AbsValue): String =
        val app = Appender()
        app >> value.toString
        val parts = value.reachableParts
        if (!parts.isEmpty) (app >> " @ ").wrap(for (part <- parts) {
          val obj = heap(part)
          app :> s"$part -> " >> obj >> LINE_SEP
        })
        app.toString

      /** getters */
      def reachable: Boolean = elem.reachable
      def locals: Map[Local, AbsValue] = elem.locals
      def globals: Map[Global, AbsValue] = elem.globals
      def heap: AbsHeap = elem.heap
    }
    // -------------------------------------------------------------------------
    // private helpers
    // -------------------------------------------------------------------------
    // appender generator
    private def mkRule(detail: Boolean): Rule[Elem] = (app, elem) =>
      val irStringifier = IRElem.getStringifier(detail, false)
      import irStringifier.given
      given Rule[AbsHeap] = if (detail) AbsHeap.rule else AbsHeap.shortRule
      if (!elem.isBottom) app.wrap {
        app :> "locals: " >> elem.locals >> LINE_SEP
        app :> "globals: " >> elem.globals >> LINE_SEP
        app :> "heaps: " >> elem.heap >> LINE_SEP
      }
      else app >> "⊥"

    // lookup ASTs
    private def lookupAst(ast: AbsAstValue, field: AbsValue): AbsValue =
      (ast.getSingle, field.getSingle) match
        case (Zero, _) | (_, Zero) => AbsValue.Bot
        case (One(AstValue(ast)), One(Str("parent"))) =>
          ast.parent.map(AbsValue(_)).getOrElse(AbsValue(Absent))
        case (One(AstValue(syn: es.Syntactic)), One(Str(fieldStr))) =>
          val es.Syntactic(name, _, rhsIdx, children) = syn
          val rhs = cfg.grammar.nameMap(name).rhsList(rhsIdx)
          rhs.getRhsIndex(fieldStr).flatMap(children(_)) match
            case Some(child) => AbsValue(child)
            case _           => AbsValue.Bot
        case (One(AstValue(syn: es.Syntactic)), One(Math(n))) if n.isValidInt =>
          syn.children(n.toInt).map(AbsValue(_)).getOrElse(AbsValue(Absent))
        case (_: One[_], _: One[_]) => AbsValue.Bot
        case _                      => exploded("ast field access")

    // lookup strings
    private def lookupStr(str: AbsStr, field: AbsValue): AbsValue =
      (str.getSingle, field.getSingle) match
        case (Zero, _) | (_, Zero) => AbsValue.Bot
        case (One(Str(str)), One(Math(k))) =>
          AbsValue(CodeUnit(str(k.toInt)))
        case (One(Str(str)), One(simple: SimpleValue)) =>
          simple match
            case Str("length") => AbsValue(Math(str.length))
            case Number(k)     => AbsValue(CodeUnit(str(k.toInt)))
            case _             => AbsValue.Bot
        case _ => AbsValue.codeUnitTop ⊔ AbsValue.mathTop
  }
}
