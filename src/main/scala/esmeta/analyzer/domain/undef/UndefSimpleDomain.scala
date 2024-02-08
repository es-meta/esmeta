package esmeta.analyzer.domain.undef

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*
import esmeta.util.*

trait UndefSimpleDomainDecl { self: Self =>

  /** simple domain for undefined values */
  object UndefSimpleDomain
    extends UndefDomain
    with SimpleDomain("undef", Fin(Undef))
}
