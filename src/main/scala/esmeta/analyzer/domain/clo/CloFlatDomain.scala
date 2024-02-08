package esmeta.analyzer.domain.clo

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

trait CloFlatDomainDecl { self: Self =>

  /** flat domain for closure values */
  object CloFlatDomain extends CloDomain with FlatDomain[AClo]("clo")
}
