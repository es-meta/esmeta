package esmeta.analyzer.domain.part

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

trait PartSetDomainDecl { self: Self =>

  /** set domain for address partitions */
  class PartSetDomain(
    maxSizeOpt: Option[Int] = None, // max size of set
  ) extends PartDomain
    with SetDomain[Part]("part", maxSizeOpt)
}
