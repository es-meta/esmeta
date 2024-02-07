package esmeta.analyzer.domain.astValue

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

trait AstValueSetDomainDecl { self: Self =>

  /** set domain for AST values */
  class AstValueSetDomain(
    maxSizeOpt: Option[Int] = None, // max size of set
  ) extends AstValueDomain
    with SetDomain[AstValue]("AST", maxSizeOpt)
}
