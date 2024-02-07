package esmeta.analyzer.domain.cont

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.Cont

trait ContDomainDecl { self: Self =>

  /** abstract continuation domain */
  trait ContDomain extends Domain[ACont] {

    /** abstraction functions for an original continuation */
    def alpha(cont: Cont): Elem = alpha(ACont.from(cont))
  }
}
