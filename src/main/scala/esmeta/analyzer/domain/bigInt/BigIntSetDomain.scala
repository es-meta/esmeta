package esmeta.analyzer.domain.bigInt

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

trait BigIntSetDomainDecl { self: Self =>

  /** set domain for big integer values */
  class BigIntSetDomain(
    maxSizeOpt: Option[Int] = None, // max size of set
  ) extends BigIntDomain
    with SetDomain[BigInt]("bigInt", maxSizeOpt)
}
