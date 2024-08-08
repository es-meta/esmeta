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
            elem.get(x) ⊑ that.get(x)
          })
          val globalsB = (lglobals.keySet ++ rglobals.keySet).forall(x => {
            elem.get(x) ⊑ that.get(x)
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
            v = elem.get(x) ⊔ that.get(x)
          } yield x -> v).toMap
          val newGlobals = (for {
            x <- (l.globals.keySet ++ r.globals.keySet).toList
            v = elem.get(x) ⊔ that.get(x)
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
            v = elem.get(x) ⊓ that.get(x)
            _ = isBottom ||= v.isBottom
          } yield x -> v).toMap
          val newGlobals = (for {
            x <- (l.globals.keySet ++ r.globals.keySet).toList
            v = elem.get(x) ⊓ that.get(x)
            _ = isBottom ||= v.isBottom
          } yield x -> v).toMap
          val newHeap = elem.heap ⊓ that.heap
          if (newHeap.isBottom || isBottom) Bot
          else Elem(true, newLocals, newGlobals, newHeap)

      // -----------------------------------------------------------------------
      // Operations for Abstract States
      // -----------------------------------------------------------------------

      /** getter */
      def get(x: Var): AbsValue = ???

      /** getter */
      def get(base: AbsValue, field: AbsValue): AbsValue = ???
      // val compValue = AbsValue(pureValue = base.comp(field.str))
      // val partValue = elem.heap(base.part, field)
      // val astValue = lookupAst(base.astValue, field)
      // val strValue = lookupStr(base.str, field)
      // compValue ⊔ partValue ⊔ astValue ⊔ strValue

      /** getter */
      def get(part: Part): AbsObj = elem.heap(part)

      /** define variables */
      def define(x: Var, value: AbsValue): Elem = x match
        case x: Global => elem.copy(globals = globals + (x -> value))
        case x: Local  => elem.copy(locals = locals + (x -> value))

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

      /** variable existence check */
      def exists(x: Var): AbsValue = ???

      /** field existence check */
      def exists(base: AbsValue, field: AbsValue): AbsValue = ???

      /** expand a field of a record object */
      def expand(base: AbsValue, field: AbsValue): Elem =
        elem.bottomCheck(AbsValue)(base, field) {
          elem.copy(heap = elem.heap.expand(base.part, field))
        }

      /** delete a key from an map object */
      def delete(base: AbsValue, field: AbsValue): Elem =
        elem.bottomCheck(AbsValue)(base, field) {
          elem.copy(heap = elem.heap.delete(base.part, field))
        }

      /** push a value to a list */
      def push(list: AbsValue, value: AbsValue, front: Boolean): Elem =
        elem.bottomCheck(AbsValue)(list, value) {
          if (front) elem.copy(heap = elem.heap.prepend(list.part, value))
          else elem.copy(heap = elem.heap.append(list.part, value))
        }

      /** pop a value from a list */
      def pop(list: AbsValue, front: Boolean): (AbsValue, Elem) =
        var v: AbsValue = AbsValue.Bot
        val st: Elem = elem.bottomCheck(AbsPart)(list.part) {
          val (newV, newH) = elem.heap.pop(list.part, front)
          v ⊔= newV
          elem.copy(heap = newH)
        }
        (v, st)

      /** copy object */
      def copy(from: AbsValue)(asite: AllocSite): (AbsValue, Elem) =
        val partV = AbsValue(asite)
        elem.bottomCheck(AbsPart)(from.part) {
          (partV, elem.copy(heap = elem.heap.copy(from.part)(asite)))
        }

      /** get keys of a record/map object as a list */
      def keys(
        obj: AbsValue,
        intSorted: Boolean,
      )(asite: AllocSite): (AbsValue, Elem) =
        val partV = AbsValue(asite)
        elem.bottomCheck(AbsPart)(obj.part) {
          (partV, elem.copy(heap = elem.heap.keys(obj.part, intSorted)(asite)))
        }

      /** allocate a record object */
      def allocRecord(
        tname: String,
        pairs: Iterable[(String, AbsValue)] = Nil,
      )(asite: AllocSite): (AbsValue, Elem) =
        val partV = AbsValue(asite)
        (partV, elem.copy(heap = heap.allocRecord(asite, tname, pairs)))

      /** allocate a map object */
      def allocMap(
        pairs: Iterable[(AbsValue, AbsValue)] = Nil,
      )(asite: AllocSite): (AbsValue, Elem) =
        val partV = AbsValue(asite)
        elem.bottomCheck(AbsValue)(
          pairs.flatMap { case (k, v) => List(k, v) },
        ) {
          (partV, elem.copy(heap = heap.allocMap(asite, pairs)))
        }

      /** allocate a list object */
      def allocList(
        vs: Iterable[AbsValue] = Nil,
      )(asite: AllocSite): (AbsValue, Elem) =
        val partV = AbsValue(asite)
        elem.bottomCheck(AbsValue)(vs) {
          (partV, elem.copy(heap = heap.allocList(asite, vs)))
        }

      /** check contains */
      def contains(list: AbsValue, value: AbsValue): AbsValue =
        heap.contains(list.part, value)

      /** handle returns (elem: return states / to: caller states) */
      def doReturn(to: Elem, lhs: Local, value: AbsValue): Elem = Elem(
        reachable = true,
        locals = to.locals + (lhs -> value),
        globals = globals,
        heap = heap.doReturn(to.heap),
      )

      /** singleton address partition checks */
      def isSingle(part: Part): Boolean = elem.heap.isSingle(part)

      /** set local environments */
      def setLocal(locals: Map[Local, AbsValue]): Elem =
        elem.copy(locals = locals)

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

      // -----------------------------------------------------------------------
      // Helpers for Debugging
      // -----------------------------------------------------------------------

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

      /** get reachable address partitions */
      def reachableParts: Set[Part] =
        var parts = Set[Part]()
        for ((_, v) <- locals) parts ++= v.reachableParts
        for ((_, v) <- globals) parts ++= v.reachableParts
        heap.reachableParts(parts)
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
    private def lookupAst(ast: AbsAstValue, field: AbsValue): AbsValue = ???
    // (ast.getSingle, field.getSingle) match
    //   case (Zero, _) | (_, Zero) => AbsValue.Bot
    //   case (One(AstValue(ast)), One(Str("parent"))) =>
    //     ast.parent.map(AbsValue(_)).getOrElse(AbsValue(Uninit))
    //   case (One(AstValue(syn: es.Syntactic)), One(Str(fieldStr))) =>
    //     val es.Syntactic(name, _, rhsIdx, children) = syn
    //     val rhs = cfg.grammar.nameMap(name).rhsList(rhsIdx)
    //     rhs.getRhsIndex(fieldStr).flatMap(children(_)) match
    //       case Some(child) => AbsValue(child)
    //       case _           => AbsValue.Bot
    //   case (One(AstValue(syn: es.Syntactic)), One(Math(n))) if n.isValidInt =>
    //     syn.children(n.toInt).map(AbsValue(_)).getOrElse(AbsValue(Uninit))
    //   case (_: One[_], _: One[_]) => AbsValue.Bot
    //   case _                      => exploded("ast field access")

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
