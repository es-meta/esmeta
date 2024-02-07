package esmeta.analyzer.domain.astValue

import esmeta.analyzer.{Analyzer, domain}

type Self = Decl & domain.Decl & Analyzer

trait Decl
  extends AstValueDomainDecl
  with AstValueSimpleDomainDecl
  with AstValueFlatDomainDecl
  with AstValueSetDomainDecl {
  self: Self =>
}
