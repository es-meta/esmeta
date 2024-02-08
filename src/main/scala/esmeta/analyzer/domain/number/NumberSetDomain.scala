package esmeta.analyzer.domain.number

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

trait NumberSetDomainDecl { self: Self =>

  /** set domain for number values */
  class NumberSetDomain(
    maxSizeOpt: Option[Int] = None, // max size of set
  ) extends NumberDomain
    with SetDomain[Number]("number", maxSizeOpt)
}
