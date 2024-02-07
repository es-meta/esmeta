package esmeta.analyzer.domain.codeUnit

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

trait CodeUnitSimpleDomainDecl { self: Self =>

  /** simple domain for code unit values */
  object CodeUnitSimpleDomain
    extends CodeUnitDomain
    with SimpleDomain[CodeUnit]("codeUnit")
}
