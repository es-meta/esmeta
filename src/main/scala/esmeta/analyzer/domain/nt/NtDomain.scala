package esmeta.analyzer.domain.nt

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.Nt

trait NtDomainDecl { self: Self =>

  /** abstract nonterminal domain */
  trait NtDomain extends Domain[Nt]
}
