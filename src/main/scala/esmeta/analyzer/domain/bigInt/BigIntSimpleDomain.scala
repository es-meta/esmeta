package esmeta.analyzer.domain.bigInt

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

trait BigIntSimpleDomainDecl { self: Self =>

  /** simple domain for big integer values */
  object BigIntSimpleDomain
    extends BigIntDomain
    with SimpleDomain[BigInt]("bigInt")
}
