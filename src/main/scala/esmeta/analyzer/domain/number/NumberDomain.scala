package esmeta.analyzer.domain.number

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

trait NumberDomainDecl { self: Self =>

  /** abstract number domain */
  trait NumberDomain extends Domain[Number]
}
