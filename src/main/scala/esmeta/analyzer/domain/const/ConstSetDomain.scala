package esmeta.analyzer.domain.const

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

trait ConstSetDomainDecl { self: Self =>

  /** set domain for constant values */
  class ConstSetDomain(
    maxSizeOpt: Option[Int] = None, // max size of set
  ) extends ConstDomain
    with SetDomain[Const]("const", maxSizeOpt)
}
