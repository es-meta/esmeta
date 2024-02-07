package esmeta.analyzer.domain.str

import esmeta.analyzer.{Analyzer, domain}

type Self = Decl & domain.Decl & Analyzer

trait Decl
  extends StrDomainDecl
  with StrSimpleDomainDecl
  with StrFlatDomainDecl
  with StrSetDomainDecl {
  self: Self =>
}
