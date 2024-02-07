package esmeta.analyzer.domain.codeUnit

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.CodeUnit

trait CodeUnitDomainDecl { self: Self =>

  /** abstract code unit domain */
  trait CodeUnitDomain extends Domain[CodeUnit]
}
