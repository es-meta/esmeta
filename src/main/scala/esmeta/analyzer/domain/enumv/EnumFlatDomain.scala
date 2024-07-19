package esmeta.analyzer.domain.enumv

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

trait EnumFlatDomainDecl { self: Self =>

  /** flat domain for enum values */
  object EnumFlatDomain extends EnumDomain with FlatDomain[Enum]("enum")
}
