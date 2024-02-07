package esmeta.analyzer.domain.cont

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

trait ContFlatDomainDecl { self: Self =>

  /** flat domain for continuation values */
  object ContFlatDomain extends ContDomain with FlatDomain[ACont]("cont")
}
