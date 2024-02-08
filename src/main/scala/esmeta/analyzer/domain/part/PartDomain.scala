package esmeta.analyzer.domain.part

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.Addr

trait PartDomainDecl { self: Self =>

  /** abstract address partition domain */
  trait PartDomain extends Domain[Part] {

    /** abstraction functions for an original address */
    def alpha(addr: Addr): Elem = alpha(Part.from(addr))
  }
}
