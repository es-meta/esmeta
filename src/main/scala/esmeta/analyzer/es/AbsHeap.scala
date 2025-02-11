package esmeta.analyzer.es

import esmeta.state.*
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*
import esmeta.util.domain.*, Lattice.*, BSet.*, Flat.*

/** abstract heaps */
trait AbsHeapDecl { self: ESAnalyzer =>
  case class AbsHeap(
    map: Map[AddrPart, AbsObj] = Map(),
    merged: Set[AddrPart] = Set(),
  ) extends DirectOps[AbsHeap]
    with Printable[AbsHeap] {
    import AbsHeap.*

    /** top element check */
    def isTop: Boolean = ???

    /** bottom element check */
    def isBottom: Boolean = map.isEmpty

    /** partial order */
    def ⊑(that: AbsHeap): Boolean = ???

    /** not partial order */
    def !⊑(that: AbsHeap): Boolean = !(this ⊑ that)

    /** join operator */
    def ⊔(that: AbsHeap): AbsHeap = ???

    /** meet operator */
    def ⊓(that: AbsHeap): AbsHeap = ???
  }
  object AbsHeap extends AbsDomain with DirectLattice {
    type Conc = Heap
    type Elem = AbsHeap

    /** top element */
    lazy val Top: AbsHeap = exploded("top abstract heap")

    /** bottom element */
    lazy val Bot: AbsHeap = AbsHeap()

    /** abstraction */
    def alpha(elems: Iterable[Heap]): AbsHeap = ???

    /** appender */
    given rule: Rule[AbsHeap] = mkRule(true)

    /** simpler appender */
    val shortRule: Rule[Elem] = mkRule(false)

    // appender generator
    private def mkRule(detail: Boolean): Rule[AbsHeap] = (app, elem) => {
      val Elem(map, merged) = elem
      if (elem.isBottom) app >> "{}"
      else if (!detail) app >> "{ ... }"
      else
        app.wrap {
          map.toList.sortBy(_._1.toString).foreach {
            case (k, v) =>
              app :> "["
              app >> (if (merged contains k) "M" else " ")
              app >> "] " >> s"$k -> " >> v
          }
        }
    }
  }
}
