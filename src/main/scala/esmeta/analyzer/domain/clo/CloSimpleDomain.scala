package esmeta.analyzer.domain.clo

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

trait CloSimpleDomainDecl { self: Self =>

  /** simple domain for closure values */
  object CloSimpleDomain extends CloDomain with SimpleDomain[AClo]("clo")
}
