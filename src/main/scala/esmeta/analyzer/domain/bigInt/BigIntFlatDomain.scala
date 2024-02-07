package esmeta.analyzer.domain.bigInt

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

trait BigIntFlatDomainDecl { self: Self =>

  /** flat domain for big integer values */
  object BigIntFlatDomain extends BigIntDomain with FlatDomain[BigInt]("bigInt")
}
