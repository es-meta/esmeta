package esmeta.analyzer.domain.uninit

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*
import esmeta.util.*

trait UninitSimpleDomainDecl { self: Self =>

  /** simple domain for uninit values */
  object UninitSimpleDomain
    extends UninitDomain
    with SimpleDomain("uninit", Fin(Uninit))
}
