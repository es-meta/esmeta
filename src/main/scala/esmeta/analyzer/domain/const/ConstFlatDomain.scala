package esmeta.analyzer.domain.const

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

trait ConstFlatDomainDecl { self: Self =>

  /** flat domain for constant values */
  object ConstFlatDomain extends ConstDomain with FlatDomain[Const]("const")
}
