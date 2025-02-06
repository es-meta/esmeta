package esmeta.analyzer.es

import esmeta.util.Appender.*

/** abstract return values */
trait AbsRetDecl { self: ESAnalyzer =>

  case class AbsRet() extends AbsRetLike {
    import AbsRet.*

    /** return value */
    def value: AbsValue = ???

    /** bottom check */
    def isBottom: Boolean = ???

    /** partial order */
    def ⊑(that: AbsRet): Boolean = ???

    /** not partial order */
    def !⊑(that: AbsRet): Boolean = ???

    /** join operator */
    def ⊔(that: AbsRet): AbsRet = ???

    /** meet operator */
    def ⊓(that: AbsRet): AbsRet = ???
  }
  object AbsRet extends DomainLike[AbsRet] {

    /** top element */
    lazy val Top: AbsRet = ???

    /** bottom element */
    lazy val Bot: AbsRet = ???

    /** appender */
    given rule: Rule[AbsRet] = (app, elem) => ???
  }
}
