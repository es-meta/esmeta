package esmeta.analyzer.domain.enumv

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

trait EnumSimpleDomainDecl { self: Self =>

  /** simple domain for enum values */
  object EnumSimpleDomain extends EnumDomain with SimpleDomain[Enum]("enum")
}
