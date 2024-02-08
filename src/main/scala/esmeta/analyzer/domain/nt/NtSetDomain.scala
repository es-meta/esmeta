package esmeta.analyzer.domain.nt

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

trait NtSetDomainDecl { self: Self =>

  /** set domain for nonterminals */
  class NtSetDomain(
    maxSizeOpt: Option[Int] = None, // max size of set
  ) extends NtDomain
    with SetDomain[Nt]("nt", maxSizeOpt)
}
