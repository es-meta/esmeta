package esmeta.analyzer.es

import esmeta.state.*
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.domain.*

/** abstract return values */
trait AbsRetDecl { self: ESAnalyzer =>

  case class AbsRet(
  ) extends Printable[AbsRet] {

    /** top element check */
    def isTop: Boolean = ???

    /** bottom element check */
    def isBottom: Boolean = ???

    /** partial order */
    def ⊑(that: AbsRet): Boolean = ???

    /** join operator */
    def ⊔(that: AbsRet): AbsRet = ???

    /** meet operator */
    def ⊓(that: AbsRet): AbsRet = ???

    /** return value */
    def value: AbsValue = ???
  }
  object AbsRet
    extends RetDomain
    with Lattice[AbsRet]
    with AbsDomain[Ret, AbsRet] {

    /** top element */
    lazy val Top: AbsRet = ???

    /** bottom element */
    lazy val Bot: AbsRet = ???

    /** abstraction */
    def alpha(elems: Iterable[Ret]): AbsRet = ???

    /** appender */
    given rule: Rule[AbsRet] = (app, elem) => ???

    extension (ret: AbsRet) {
      def value: AbsValue = ret.value
    }
  }

  type Ret = (Value, State)

  given Lattice.Ops[AbsRet] with
    extension (x: AbsRet) {
      def isTop: Boolean = ???
      def isBottom: Boolean = ???
      def ⊑(y: AbsRet): Boolean = ???
      def ⊔(y: AbsRet): AbsRet = ???
      def ⊓(y: AbsRet): AbsRet = ???
    }

  given AbsDomain.GenericOps[Ret, AbsRet] with
    extension (x: AbsRet) {
      def contains(value: Ret): Boolean = ???
      def toBSet: BSet[Ret] = ???
      def toFlat: Flat[Ret] = ???
    }
}
