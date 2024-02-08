package esmeta.analyzer.domain.nullv

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

trait NullDomainDecl { self: Self =>

  /** abstract null domain */
  trait NullDomain extends Domain[Null]
}
