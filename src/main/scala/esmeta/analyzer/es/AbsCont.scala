package esmeta.analyzer.es

import esmeta.es.*
import esmeta.state.*
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.domain.*, Lattice.*, BSet.*, Flat.*

/** abstract primitive values */
trait AbsContDecl { self: ESAnalyzer =>

  // TODO more precise abstraction
  case class AbsCont(exist: Boolean)
    extends DirectOps[AbsCont]
    with Printable[AbsCont] {

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
  object AbsCont extends AbsDomain with DirectLattice {
    type Conc = Cont
    type Elem = AbsCont

    /** top element */
    lazy val Top: AbsCont = AbsCont(true)

    /** bottom element */
    lazy val Bot: AbsCont = AbsCont(false)

    /** abstraction */
    def alpha(elems: Iterable[Cont]): AbsCont = ???

    /** appender */
    given rule: Rule[AbsCont] = (app, elem) => ???
  }
}
