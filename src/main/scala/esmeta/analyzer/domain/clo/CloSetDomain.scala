package esmeta.analyzer.domain.clo

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

trait CloSetDomainDecl { self: Self =>

  /** set domain for closure values */
  class CloSetDomain(
    maxSizeOpt: Option[Int] = None, // max size of set
  ) extends CloDomain
    with SetDomain[AClo]("clo", maxSizeOpt)
}
