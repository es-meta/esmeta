package esmeta.analyzer.es

import esmeta.es.*
import esmeta.state.*
import esmeta.util.*
import esmeta.util.Appender.{*, given}
import esmeta.util.BaseUtils.*
import esmeta.util.domain.*, Lattice.*, BSet.*, Flat.*

/** abstract primitive values */
trait AbsAddrDecl { self: ESAnalyzer =>

  /** abstract address */
  type AbsAddr = AbsAddr.Elem
  object AbsAddr extends SetDomain[AddrPart] {

    /** appender */
    given rule: Rule[AbsAddr] = setRule
  }

  /** allocation site */
  enum AddrPart:
    case Named(name: String)
    case Alloc(k: Int, view: View)
  object AddrPart {
    def apply(addr: Addr): AddrPart = addr match
      case NamedAddr(name) => Named(name)
      case DynamicAddr(k)  => error(s"invalid address abstraction: $addr")
  }
  given Rule[AddrPart] = (app, addr) => {
    import AddrPart.*
    addr match
      case Named(name)    => app >> "#" >> name
      case Alloc(k, view) => app >> "#" >> k >> ":" >> view.toString
  }
  given Ordering[AddrPart] = Ordering.by(_.toString)
}
