package esmeta.analyzer.domain.part

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

trait PartSimpleDomainDecl { self: Self =>

  /** simple domain for address partitions */
  object PartSimpleDomain extends PartDomain with SimpleDomain[Part]("part")
}
