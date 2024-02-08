package esmeta.analyzer.domain.const

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.Const

trait ConstDomainDecl { self: Self =>

  /** abstract constant domain */
  trait ConstDomain extends Domain[Const]
}
