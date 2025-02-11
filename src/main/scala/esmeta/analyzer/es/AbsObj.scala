package esmeta.analyzer.es

import esmeta.state.*
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*
import esmeta.util.domain.*, Lattice.*, BSet.*, Flat.*

/** abstract heaps */
trait AbsObjDecl { self: ESAnalyzer =>
  case class AbsObj() extends DirectOps[AbsObj] with Printable[AbsObj] {
    import AbsObj.*

    /** top element check */
    def isTop: Boolean = ???

    /** bottom element check */
    def isBottom: Boolean = ???

    /** partial order */
    def ⊑(that: AbsObj): Boolean = ???

    /** not partial order */
    def !⊑(that: AbsObj): Boolean = !(this ⊑ that)

    /** join operator */
    def ⊔(that: AbsObj): AbsObj = ???

    /** meet operator */
    def ⊓(that: AbsObj): AbsObj = ???
  }
  object AbsObj extends AbsDomain with DirectLattice {
    type Conc = Obj
    type Elem = AbsObj

    /** top element */
    lazy val Top: AbsObj = exploded("top abstract heap")

    /** bottom element */
    val Bot: AbsObj = ???

    /** abstraction */
    def alpha(elems: Iterable[Obj]): AbsObj = ???

    /** appender */
    given rule: Rule[AbsObj] = mkRule(true)

    /** simpler appender */
    val shortRule: Rule[Elem] = mkRule(false)

    // appender generator
    private def mkRule(detail: Boolean): Rule[AbsObj] = (app, elem) => ???
  }
}
