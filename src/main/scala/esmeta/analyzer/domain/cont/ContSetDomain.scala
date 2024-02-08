package esmeta.analyzer.domain.cont

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

trait ContSetDomainDecl { self: Self =>

  /** set domain for continuation values */
  class ContSetDomain(
    maxSizeOpt: Option[Int] = None, // max size of set
  ) extends ContDomain
    with SetDomain[ACont]("cont", maxSizeOpt)
}
