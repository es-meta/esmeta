package esmeta.analyzer

import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*

trait DomainDecl { self: Analyzer =>

  /** abstract domain */
  trait DomainLike[Elem] {

    /** top element */
    def Top: Elem

    /** bottom element */
    def Bot: Elem

    /** appender */
    given rule: Rule[Elem]
  }

  trait DomainElemLike[Elem] { self: Elem =>

    /** abstract domain */
    def domain: DomainLike[Elem]

    /** conversion to string */
    override def toString: String = stringify(this)(using domain.rule)
  }

  /** abstract values */
  trait AbsValueLike extends DomainElemLike[AbsValue] { self: AbsValue =>

    /** abstract domain */
    def domain = AbsValue

    /** get string of abstract value with an abstract state */
    def getString(state: AbsState): String
  }
  val AbsValue: DomainLike[AbsValue]

  /** abstract states */
  trait AbsStateLike extends DomainElemLike[AbsState] { self: AbsState =>

    /** abstract domain */
    def domain = AbsState

    /** has imprecise elements */
    def hasImprec: Boolean
  }
  val AbsState: DomainLike[AbsState]

  /** abstract return values */
  trait AbsRetLike extends DomainElemLike[AbsRet] { self: AbsRet =>

    /** abstract domain */
    def domain = AbsRet

    /** return value */
    def value: AbsValue
  }
  val AbsRet: DomainLike[AbsRet]
}
