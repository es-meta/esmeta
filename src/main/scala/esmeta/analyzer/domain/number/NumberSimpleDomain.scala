package esmeta.analyzer.domain.number

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

trait NumberSimpleDomainDecl { self: Self =>

  /** simple domain for number values */
  object NumberSimpleDomain
    extends NumberDomain
    with SimpleDomain[Number]("number")
}
