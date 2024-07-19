package esmeta.analyzer.domain.enumv

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.Enum

trait EnumDomainDecl { self: Self =>

  /** abstract enum domain */
  trait EnumDomain extends Domain[Enum]
}
