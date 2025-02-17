package esmeta.analyzer.es

import esmeta.state.*
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.domain.*

/** abstract return values */
trait AbsRetDecl { self: ESAnalyzer =>

  case class AbsRet(
    value: AbsValue = AbsValue.Bot,
    state: AbsState = AbsState.Bot,
  ) extends Printable[AbsRet]

  object AbsRet
    extends RetDomain
    with Lattice[AbsRet]
    with AbsDomain[Ret, AbsRet] {

    /** top element */
    lazy val Top: AbsRet = AbsRet(AbsValue.Top, AbsState.Top)

    /** bottom element */
    lazy val Bot: AbsRet = AbsRet()

    /** abstraction */
    def alpha(elems: Iterable[Ret]): AbsRet = ???

    /** appender */
    given rule: Rule[AbsRet] = (app, ret) =>
      app >> ret.value.getString(ret.state)

    extension (ret: AbsRet) {
      def value: AbsValue = ret.value
    }
  }

  type Ret = (Value, State)

  given Lattice.Ops[AbsRet] with
    extension (x: AbsRet) {
      def isTop: Boolean = x.value.isTop && x.state.isTop
      def isBottom: Boolean = x.value.isBottom && x.state.isBottom
      def ⊑(y: AbsRet): Boolean = (x.value ⊑ y.value) && (x.state ⊑ y.state)
      def ⊔(y: AbsRet): AbsRet = AbsRet(x.value ⊔ y.value, x.state ⊔ y.state)
      def ⊓(y: AbsRet): AbsRet = AbsRet(x.value ⊓ y.value, x.state ⊓ y.state)
    }

  given AbsDomain.GenericOps[Ret, AbsRet] with
    extension (x: AbsRet) {
      def contains(value: Ret): Boolean = ???
      def toBSet: BSet[Ret] = ???
      def toFlat: Flat[Ret] = ???
    }
}
