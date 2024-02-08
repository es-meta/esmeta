package esmeta.analyzer.domain.astValue

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

trait AstValueSimpleDomainDecl { self: Self =>

  /** simple domain for AST values */
  object AstValueSimpleDomain
    extends AstValueDomain
    with SimpleDomain[AstValue]("AST")
}
