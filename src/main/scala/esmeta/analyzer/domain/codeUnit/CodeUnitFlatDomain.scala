package esmeta.analyzer.domain.codeUnit

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

trait CodeUnitFlatDomainDecl { self: Self =>

  /** flat domain for code unit values */
  object CodeUnitFlatDomain
    extends CodeUnitDomain
    with FlatDomain[CodeUnit]("codeUnit")
}
