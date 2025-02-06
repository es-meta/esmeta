package esmeta.analyzer.es

import esmeta.es.*
import esmeta.state.*
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.domain.{*, given}, BSet.*, Flat.*

/** abstract primitive values */
trait AbsAddrDecl { self: ESAnalyzer =>

  case class AbsAddr(set: BSet[Addr]) extends DomainElemLike[AbsAddr] {

    /** abstract domain */
    def domain = AbsAddr

    /** bottom check */
    def isBottom: Boolean = ???

    /** partial order */
    def ⊑(that: AbsAddr): Boolean = ???

    /** not partial order */
    def !⊑(that: AbsAddr): Boolean = !(this ⊑ that)

    /** join operator */
    def ⊔(that: AbsAddr): AbsAddr = ???

    /** meet operator */
    def ⊓(that: AbsAddr): AbsAddr = ???
  }
  object AbsAddr extends DomainLike[AbsAddr] {

    /** top element */
    lazy val Top: AbsAddr = ???

    /** bottom element */
    lazy val Bot: AbsAddr = ???

    // abstraction functions
    def apply(addrs: Addr*): AbsAddr = apply(addrs)
    def apply(addrs: Iterable[Addr]): AbsAddr = AbsAddr(BSet(addrs))

    /** appender */
    given rule: Rule[AbsAddr] = (app, elem) => ???
  }

  /** allocation site */
  enum AllocSite:
    case Static(name: String)
    case Dynamic(k: Int, view: View)
}
