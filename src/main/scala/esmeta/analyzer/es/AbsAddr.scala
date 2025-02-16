package esmeta.analyzer.es

import esmeta.es.*
import esmeta.state.*
import esmeta.util.*
import esmeta.util.Appender.{*, given}
import esmeta.util.BaseUtils.*
import esmeta.domain.{*, given}

/** abstract primitive values */
trait AbsAddrDecl { self: ESAnalyzer =>

  /** abstract address */
  type AbsAddr = AbsAddr.Elem

  object AbsAddr extends SetDomain[AddrPart] {
    def alpha(addr: Addr): Elem = Set(AddrPart(addr))
  }

  given Rule[AbsAddr] = setRule("", " | ", "")

  /** address partitions */
  enum AddrPart:
    case Named(name: String)
    case ASite(k: Int, view: View)
  object AddrPart {
    def apply(addr: Addr): AddrPart = addr match
      case NamedAddr(name) => Named(name)
      case DynamicAddr(k)  => error(s"invalid address abstraction: $addr")
  }

  given Rule[AddrPart] = (app, addr) => {
    import AddrPart.*
    addr match
      case Named(name)    => app >> "#" >> name
      case ASite(k, view) => app >> "#" >> k >> ":" >> view
  }

  given Ordering[AddrPart] = Ordering.by(_.toString)
}
