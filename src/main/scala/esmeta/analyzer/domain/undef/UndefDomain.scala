package esmeta.analyzer.domain.undef

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

trait UndefDomainDecl { self: Self =>

  /** abstract undefined domain */
  trait UndefDomain extends Domain[Undef]
}
