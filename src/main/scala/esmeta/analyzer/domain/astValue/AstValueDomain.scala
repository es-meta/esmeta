package esmeta.analyzer.domain.astValue

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.AstValue

trait AstValueDomainDecl { self: Self =>

  /** abstract AST domain */
  trait AstValueDomain extends Domain[AstValue]
}
