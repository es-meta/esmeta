package esmeta.analyzer

import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*

trait DomainDecl { self: Analyzer =>

  /** analysis domains */
  trait AnalysisDomain[Elem <: AnalysisElem[Elem]] {

    /** bottom element */
    def Bot: Elem

    /** appender */
    given rule: Rule[Elem]
  }

  /** analysis elements */
  trait AnalysisElem[Elem <: AnalysisElem[Elem]] { self: Elem =>

    /** abstract domain */
    def domain: AnalysisDomain[Elem]

    /** conversion to string */
    override def toString: String = stringify(this)(using domain.rule)
  }

  /** value domains */
  trait ValueDomain extends AnalysisDomain[AbsValue]
  val AbsValue: ValueDomain

  /** abstract values */
  trait AbsValueElem extends AnalysisElem[AbsValue] { self: AbsValue =>

    /** abstract domain */
    def domain = AbsValue

    /** get string of abstract value with an abstract state */
    def getString(state: AbsState): String
  }
  type AbsValue <: AbsValueElem

  /** state domains */
  trait StateDomain extends AnalysisDomain[AbsState]
  val AbsState: StateDomain

  /** abstract states */
  trait AbsStateElem extends AnalysisElem[AbsState] { self: AbsState =>

    /** abstract domain */
    def domain = AbsState

    /** has imprecise elements */
    def hasImprec: Boolean
  }
  type AbsState <: AbsStateElem

  /** return value domains */
  trait RetDomain extends AnalysisDomain[AbsRet]
  val AbsRet: RetDomain

  /** abstract return values */
  trait AbsRetElem extends AnalysisElem[AbsRet] { self: AbsRet =>

    /** abstract domain */
    def domain = AbsRet

    /** return value */
    def value: AbsValue
  }
  type AbsRet <: AbsRetElem
}
