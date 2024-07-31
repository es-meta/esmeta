package esmeta.analyzer.domain.uninit

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

trait UninitDomainDecl { self: Self =>

  /** abstract uninit domain */
  trait UninitDomain extends Domain[Uninit]
}
