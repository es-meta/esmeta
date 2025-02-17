package esmeta.analyzer.es

import esmeta.*
import esmeta.es.*
import esmeta.ir.{*, given}
import esmeta.state.*
import esmeta.ty.ValueTy
import esmeta.util.*
import esmeta.util.Appender.{*, given}
import esmeta.util.BaseUtils.*
import esmeta.domain.{*, given}, BSet.*, Flat.*

/** abstract states */
trait AbsStateDecl { self: ESAnalyzer =>
  case class AbsState(
    reachable: Boolean = false,
    locals: Map[Local, AbsValue.opt] = Map(),
    globals: Map[Global, AbsValue] = Map(),
    heap: AbsHeap = AbsHeap.Bot,
  ) extends Printable[AbsState] {
    import AbsState.*
    given AbsState = this

    /** getter */
    def get(rt: AbsRefTarget): AbsValue = rt match
      case AbsRefTarget.AbsId(x)              => get(x)
      case AbsRefTarget.AbsField(base, field) => get(base, field)

    /** getter */
    def get(x: Var): AbsValue = x match
      case x: Global => globals.getOrElse(x, base.getOrElse(x, AbsValue.Bot))
      case x: Local  => locals.get(x).fold(AbsValue.Bot)(_.value)

    /** getter */
    def get(
      base: AbsValue,
      field: AbsValue,
    )(using AbsState): AbsValue =
      base.addr.foldLeft(AbsValue.Bot) { _ ⊔ heap(_)(field) }

    /** define variables */
    def define(
      x: Var,
      value: AbsValue,
    ): AbsState = this.checkBottom(value)(x match
      case x: Local  => copy(locals = locals + (x -> value.opt))
      case x: Global => copy(globals = globals + (x -> value)),
    )

    /** setter */
    def update(
      rt: AbsRefTarget,
      value: AbsValue,
    ): AbsState = rt match
      case AbsRefTarget.AbsId(x)              => update(x, value)
      case AbsRefTarget.AbsField(base, field) => update(base, field, value)

    /** setter */
    def update(
      x: Var,
      value: AbsValue,
    ): AbsState = define(x, value)

    /** setter */
    def update(
      base: AbsValue,
      field: AbsValue,
      value: AbsValue,
    ): AbsState = this.checkBottom(value) {
      val addr = base.addr
      val h = heap.update(addr, field, value)
      this.checkBottom(h) { copy(heap = h) }
    }

    /** type check */
    def typeCheck(
      value: AbsValue,
      givenTy: ValueTy,
    ): ValueTy = ???

    /** existence check */
    def exists(
      rt: AbsRefTarget,
    ): AbsValue =
      import AbsRefTarget.*
      AbsValue(prim = AbsPrimValue(bool = rt match {
        case AbsId(x)              => exists(x)
        case AbsField(base, field) => exists(base, field)
      }))

    /** variable existence check */
    def exists(x: Var): Flat[Boolean] = x match
      case x: Global => Flat.Many
      case x: Local  => locals.getOrElse(x, AbsValue.opt.Absent).exists

    /** field existence check */
    def exists(
      base: AbsValue,
      field: AbsValue,
    ): Flat[Boolean] = {
      heap.exists(base.addr, field) ⊔
      base.ast.flatMap(exists(_, field))
    }

    /** AST field existence check */
    def exists(
      ast: Ast,
      field: AbsValue,
    ): Flat[Boolean] = ???

    /** expand a field of a record object */
    def expand(
      base: AbsValue,
      field: AbsValue,
    ): AbsState = ???

    /** delete a key from an map object */
    def delete(
      base: AbsValue,
      field: AbsValue,
    ): AbsState = ???

    /** push a value to a list */
    def push(
      list: AbsValue,
      value: AbsValue,
      front: Boolean,
    ): AbsState = ???

    /** pop a value from a list */
    def pop(
      part: AddrPart,
      list: AbsValue,
      front: Boolean,
    ): AbsState = ???

    /** copy object */
    def copyObj(
      part: AddrPart,
      from: AbsValue,
    ): AbsState = ???

    /** get keys of a record/map object as a list */
    def keys(
      part: AddrPart,
      base: AbsValue,
      intSorted: Boolean,
    ): AbsState = ???

    /** allocate a record object */
    def allocRecord(
      part: AddrPart,
      tname: String,
      pairs: Iterable[(String, AbsValue)],
    ): AbsState =
      val h = heap.allocRecord(part, tname, pairs)
      this.checkBottom(h) { copy(heap = h) }

    /** allocate a map object */
    def allocMap(
      pairs: Iterable[(AbsValue, AbsValue)],
      part: AddrPart,
    ): AbsState = ???

    /** allocate a list object */
    def allocList(
      part: AddrPart,
      vs: Iterable[AbsValue],
    ): AbsState = ???

    /** handle returns */
    def doReturn(to: Elem, defs: (Local, AbsValue.opt)*): Elem =
      doReturn(to, defs)

    /** handle returns */
    def doReturn(
      callerSt: AbsState,
      defs: Iterable[(Local, AbsValue.opt)],
    ): AbsState = AbsState(
      reachable = true,
      locals = callerSt.locals ++ defs,
      globals = globals,
      heap = heap.doReturn(callerSt.heap),
    ).garbageCollected

    /** TODO garbage collection */
    def garbageCollected: AbsState = this
  }
  object AbsState
    extends StateDomain
    with Lattice[AbsState]
    with AbsDomain[State, AbsState] {

    /** bases */
    private lazy val base: Map[Global, AbsValue] =
      for ((x, v) <- cfg.init.initGlobal) yield x -> AbsValue(v)

    /** top element */
    lazy val Top: AbsState = exploded("top abstract state")

    /** bottom element */
    lazy val Bot: AbsState = AbsState()

    /** empty element */
    lazy val Empty: AbsState = AbsState(reachable = true)

    /** abstraction */
    def alpha(elems: Iterable[State]): AbsState = ???

    // appender generator
    def mkRule(detail: Boolean): Rule[AbsState] = (app, elem) => {
      import irStringifier.given
      given Rule[AbsHeap] = AbsHeap.mkRule(detail)
      if (!elem.isBottom) app.wrap {
        app :> "locals: " >> elem.locals
        app :> "globals: " >> elem.globals
        app :> "heaps: " >> elem.heap
      }
      else app >> "⊥"
    }

    extension (state: AbsState) {
      def hasImprec: Boolean = ???
    }
  }

  given Lattice[AbsState] = AbsState

  given Lattice.Ops[AbsState] with
    extension (x: AbsState) {

      def isTop: Boolean = false

      def isBottom: Boolean = x.reachable == false

      def ⊑(y: AbsState): Boolean = {
        if (!x.reachable) true
        else if (!y.reachable) false
        else {
          ???
        }
      }

      def ⊔(y: AbsState): AbsState = {
        if (!x.reachable) y
        else if (!y.reachable) x
        else {
          ???
        }
      }
      def ⊓(y: AbsState): AbsState = ???
    }

  given AbsDomain.GenericOps[State, AbsState] with
    extension (st: AbsState) {
      def contains(value: State): Boolean = ???
      def toBSet: BSet[State] = ???
      def toFlat: Flat[State] = ???
    }

  given Rule[AbsState] = AbsState.mkRule(true)
}
