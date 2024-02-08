package esmeta.analyzer.domain.cont

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

trait ContSimpleDomainDecl { self: Self =>

  /** simple domain for continuation values */
  object ContSimpleDomain extends ContDomain with SimpleDomain[ACont]("cont")
}
