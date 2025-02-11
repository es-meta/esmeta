package esmeta.analyzer.es

import esmeta.*
import esmeta.ir.{*, given}
import esmeta.state.*
import esmeta.ty.ValueTy
import esmeta.util.*
import esmeta.util.Appender.{*, given}
import esmeta.util.BaseUtils.*
import esmeta.util.domain.*, Lattice.*, BSet.*, Flat.*

/** abstract states */
trait AbsStateDecl { self: ESAnalyzer =>
  case class AbsState(
    reachable: Boolean = false,
    locals: Map[Local, AbsValue] = Map(),
    globals: Map[Global, AbsValue] = Map(),
    heap: AbsHeap = AbsHeap.Bot,
  ) extends DirectOps[AbsState]
    with Printable[AbsState] {
    import AbsState.*, Lattice.{*, given}

    given AbsState = this

    /** top element check */
    def isTop: Boolean = ???

    /** bottom element check */
    def isBottom: Boolean = !reachable

    /** partial order */
    def ⊑(that: AbsState): Boolean =
      if (this.isBottom) true
      else if (that.isBottom) false
      else {
        val AbsState(_, llocals, lglobals, lheap) = this
        val AbsState(_, rlocals, rglobals, rheap) = that
        ???
      }

    /** not partial order */
    def !⊑(that: AbsState): Boolean = !(this ⊑ that)

    /** join operator */
    def ⊔(that: AbsState): AbsState = ???

    /** meet operator */
    def ⊓(that: AbsState): AbsState = ???

    /** getter */
    def get(
      x: Var,
    ): AbsValue = ???

    /** getter */
    def get(
      base: AbsValue,
      field: AbsValue,
    )(using AbsState): AbsValue = ???

    /** define variables */
    def define(
      x: Var,
      value: AbsValue,
    ): AbsState = x match {
      case x: Local  => copy(locals = locals + (x -> value))
      case x: Global => copy(globals = globals + (x -> value))
    }

    /** identifier setter */
    def update(
      x: Var,
      value: AbsValue,
      refine: Boolean,
    ): AbsState = ???

    /** type check */
    def typeCheck(
      value: AbsValue,
      givenTy: ValueTy,
    ): ValueTy = ???

    /** variable existence check */
    def exists(
      ref: Ref,
    ): AbsValue = ???

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
      list: AbsValue,
      front: Boolean,
    ): (AbsValue, AbsState) = ???

    /** copy object */
    def copy(
      from: AbsValue,
    ): (AbsValue, AbsState) = ???

    /** get keys of a record/map object as a list */
    def keys(
      base: AbsValue,
      intSorted: Boolean,
    ): (AbsValue, AbsState) = ???

    /** allocate a record object */
    def allocRecord(
      tname: String,
      pairs: Iterable[(String, AbsValue)],
    ): (AbsValue, AbsState) = ???

    /** allocate a map object */
    def allocMap(
      pairs: Iterable[(AbsValue, AbsValue)],
    ): (AbsValue, AbsState) = ???

    /** allocate a list object */
    def allocList(
      vs: Iterable[AbsValue],
    ): (AbsValue, AbsState) = ???
  }
  object AbsState extends StateDomain {

    /** bases */
    private lazy val globals: Map[Global, AbsValue] =
      for ((x, v) <- cfg.init.initGlobal) yield x -> AbsValue(v)

    /** top element */
    lazy val Top: AbsState = exploded("top abstract state")

    /** bottom element */
    lazy val Bot: AbsState = AbsState()

    /** empty element */
    lazy val Empty: AbsState = AbsState(reachable = true)

    /** appender */
    given rule: Rule[AbsState] = mkRule(true)

    // appender generator
    private def mkRule(detail: Boolean): Rule[AbsState] = (app, elem) => {
      import irStringifier.given
      given Rule[AbsHeap] = if (detail) AbsHeap.rule else AbsHeap.shortRule
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
}
