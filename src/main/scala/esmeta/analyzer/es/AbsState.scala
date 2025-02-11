package esmeta.analyzer.es

import esmeta.ir.*
import esmeta.state.*
import esmeta.ty.ValueTy
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.Appender.{*, given}
import esmeta.util.BaseUtils.*

/** abstract states */
trait AbsStateDecl { self: ESAnalyzer =>
  case class AbsState() {
    import AbsState.*

    given AbsState = this

    /** bottom check */
    def isBottom: Boolean = ???

    /** partial order */
    def ⊑(that: AbsState): Boolean = ???

    /** not partial order */
    def !⊑(that: AbsState): Boolean = !(this ⊑ that)

    /** join operator */
    def ⊔(that: AbsState): AbsState = ???

    /** meet operator */
    def ⊓(that: AbsState): AbsState = ???

    /** getter */
    def get(x: Var): AbsValue = ???

    /** getter */
    def get(base: AbsValue, field: AbsValue)(using AbsState): AbsValue = ???

    /** define variables */
    def define(x: Var, value: AbsValue): AbsState = ???

    /** identifier setter */
    def update(x: Var, value: AbsValue, refine: Boolean): AbsState = ???

    /** type check */
    def typeCheck(value: AbsValue, givenTy: ValueTy): ValueTy = ???

    /** variable existence check */
    def exists(ref: Ref): AbsValue = ???

    /** expand a field of a record object */
    def expand(base: AbsValue, field: AbsValue): AbsState = ???

    /** delete a key from an map object */
    def delete(base: AbsValue, field: AbsValue): AbsState = ???

    /** push a value to a list */
    def push(list: AbsValue, value: AbsValue, front: Boolean): AbsState = ???

    /** pop a value from a list */
    def pop(list: AbsValue, front: Boolean): (AbsValue, AbsState) = ???

    /** copy object */
    def copy(from: AbsValue): (AbsValue, AbsState) = ???

    /** get keys of a record/map object as a list */
    def keys(base: AbsValue, intSorted: Boolean): (AbsValue, AbsState) = ???

    /** allocate a record object */
    def allocRecord(
      tname: String,
      pairs: Iterable[(String, AbsValue)],
    ): (AbsValue, AbsState) = ???

    /** allocate a map object */
    def allocMap(pairs: Iterable[(AbsValue, AbsValue)]): (AbsValue, AbsState) =
      ???

    /** allocate a list object */
    def allocList(vs: Iterable[AbsValue]): (AbsValue, AbsState) = ???
  }
  object AbsState extends StateDomain {

    /** bases */
    private val globals: Map[Global, AbsValue] =
      for ((x, v) <- cfg.init.initGlobal.toMap) yield x -> AbsValue(v)

    /** top element */
    lazy val Top: AbsState = ???

    /** bottom element */
    lazy val Bot: AbsState = ???

    /** empty element */
    lazy val Empty: AbsState = ???

    /** appender */
    given rule: Rule[AbsState] = mkRule(true)

    // appender generator
    private def mkRule(detail: Boolean): Rule[AbsState] = (app, elem) => ???

    extension (state: AbsState) {
      def hasImprec: Boolean = ???
    }
  }
}
