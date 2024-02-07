package esmeta.analyzer.domain.str

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

trait StrSetDomainDecl { self: Self =>

  /** set domain for string values */
  class StrSetDomain(
    maxSizeOpt: Option[Int] = None, // max size of set
  ) extends StrDomain
    with SetDomain[Str]("str", maxSizeOpt)
}
