package esmeta.analyzer.domain.part

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

trait PartFlatDomainDecl { self: Self =>

  /** flat domain for address partitions */
  object PartFlatDomain extends PartDomain with FlatDomain[Part]("part")
}
