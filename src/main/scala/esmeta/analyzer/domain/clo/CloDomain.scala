package esmeta.analyzer.domain.clo

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.Clo

trait CloDomainDecl { self: Self =>

  /** abstract closure domain */
  trait CloDomain extends Domain[AClo] {

    /** abstraction functions for an original closure */
    def alpha(clo: Clo): Elem = alpha(AClo.from(clo))
  }
}
