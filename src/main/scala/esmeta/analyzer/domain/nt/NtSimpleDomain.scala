package esmeta.analyzer.domain.nt

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

trait NtSimpleDomainDecl { self: Self =>

  /** simple domain for nonterminals */
  object NtSimpleDomain extends NtDomain with SimpleDomain[Nt]("nt")
}
