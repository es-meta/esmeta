package esmeta.analyzer.domain.nt

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

trait NtFlatDomainDecl { self: Self =>

  /** flat domain for nonterminals */
  object NtFlatDomain extends NtDomain with FlatDomain[Nt]("nt")
}
