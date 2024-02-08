package esmeta.analyzer.domain.number

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

trait NumberFlatDomainDecl { self: Self =>

  /** flat domain for number values */
  object NumberFlatDomain extends NumberDomain with FlatDomain[Number]("number")
}
