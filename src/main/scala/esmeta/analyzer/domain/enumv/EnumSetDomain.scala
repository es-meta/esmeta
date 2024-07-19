package esmeta.analyzer.domain.enumv

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

trait EnumSetDomainDecl { self: Self =>

  /** set domain for enum values */
  class EnumSetDomain(
    maxSizeOpt: Option[Int] = None, // max size of set
  ) extends EnumDomain
    with SetDomain[Enum]("enum", maxSizeOpt)
}
