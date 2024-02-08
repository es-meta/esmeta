package esmeta.analyzer.domain.const

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

trait ConstSimpleDomainDecl { self: Self =>

  /** simple domain for constant values */
  object ConstSimpleDomain extends ConstDomain with SimpleDomain[Const]("const")
}
