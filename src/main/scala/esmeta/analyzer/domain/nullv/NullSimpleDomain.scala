package esmeta.analyzer.domain.nullv

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*
import esmeta.util.*

trait NullSimpleDomainDecl { self: Self =>

  /** simple domain for null values */
  object NullSimpleDomain
    extends NullDomain
    with SimpleDomain("null", Fin(Null))
}
