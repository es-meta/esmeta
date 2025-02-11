package esmeta.analyzer.es

import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.domain.*, Lattice.*, BSet.*, Flat.*

/** abstract return values */
trait AbsRetDecl { self: ESAnalyzer =>

  case class AbsRet() extends DirectOps[AbsRet] with Printable[AbsRet] {

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
  object AbsRet extends RetDomain {

    /** top element */
    lazy val Top: AbsRet = ???

    /** bottom element */
    lazy val Bot: AbsRet = ???

    /** appender */
    given rule: Rule[AbsRet] = (app, elem) => ???

    extension (ret: AbsRet) {
      def value: AbsValue = ret.value
    }
  }
}
