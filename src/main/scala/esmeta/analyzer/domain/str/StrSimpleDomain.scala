package esmeta.analyzer.domain.str

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

trait StrSimpleDomainDecl { self: Self =>

  /** simple domain for string values */
  object StrSimpleDomain extends StrDomain with SimpleDomain[Str]("str")
}
