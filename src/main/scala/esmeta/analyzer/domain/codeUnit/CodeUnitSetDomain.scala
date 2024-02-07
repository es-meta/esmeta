package esmeta.analyzer.domain.codeUnit

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

trait CodeUnitSetDomainDecl { self: Self =>

  /** set domain for code unit values */
  class CodeUnitSetDomain(
    maxSizeOpt: Option[Int] = None, // max size of set
  ) extends CodeUnitDomain
    with SetDomain[CodeUnit]("codeUnit", maxSizeOpt)
}
