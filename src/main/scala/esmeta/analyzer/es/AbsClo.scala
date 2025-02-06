package esmeta.analyzer.es

import esmeta.es.*
import esmeta.state.*
import esmeta.util.*
import esmeta.util.Appender.*

/** abstract primitive values */
trait AbsCloDecl { self: ESAnalyzer =>

  // TODO more precise abstraction
  case class AbsClo(exist: Boolean) extends DomainElemLike[AbsClo] {

    /** abstract domain */
    def domain = AbsClo

    /** bottom check */
    def isTop: Boolean = exist

    /** bottom check */
    def isBottom: Boolean = !exist

    /** partial order */
    def ⊑(that: AbsClo): Boolean = !exist || that.exist

    /** not partial order */
    def !⊑(that: AbsClo): Boolean = !(this ⊑ that)

    /** join operator */
    def ⊔(that: AbsClo): AbsClo = AbsClo(exist || that.exist)

    /** meet operator */
    def ⊓(that: AbsClo): AbsClo = AbsClo(exist && that.exist)
  }
  object AbsClo extends DomainLike[AbsClo] {

    /** top element */
    lazy val Top: AbsClo = AbsClo(true)

    /** bottom element */
    lazy val Bot: AbsClo = AbsClo(false)

    /** appender */
    given rule: Rule[AbsClo] = (app, elem) => ???
  }
}
