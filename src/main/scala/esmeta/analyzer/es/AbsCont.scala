package esmeta.analyzer.es

import esmeta.es.*
import esmeta.state.*
import esmeta.util.*
import esmeta.util.Appender.*

/** abstract primitive values */
trait AbsContDecl { self: ESAnalyzer =>

  // TODO more precise abstraction
  case class AbsCont(exist: Boolean) extends AnalysisElem[AbsCont] {

    /** abstract domain */
    def domain = AbsCont

    /** bottom check */
    def isTop: Boolean = exist

    /** bottom check */
    def isBottom: Boolean = !exist

    /** partial order */
    def ⊑(that: AbsCont): Boolean = !exist || that.exist

    /** not partial order */
    def !⊑(that: AbsCont): Boolean = !(this ⊑ that)

    /** join operator */
    def ⊔(that: AbsCont): AbsCont = AbsCont(exist || that.exist)

    /** meet operator */
    def ⊓(that: AbsCont): AbsCont = AbsCont(exist && that.exist)
  }
  object AbsCont extends AnalysisDomain[AbsCont] {

    /** top element */
    lazy val Top: AbsCont = AbsCont(true)

    /** bottom element */
    lazy val Bot: AbsCont = AbsCont(false)

    /** appender */
    given rule: Rule[AbsCont] = (app, elem) => ???
  }
}
