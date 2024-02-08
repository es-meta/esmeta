package esmeta.analyzer.domain.value

import esmeta.analyzer.{Analyzer, domain}

type Self = Decl & domain.Decl & Analyzer

trait Decl
  extends ValueDomainDecl
  with ValueBasicDomainDecl
  with ValueTypeDomainDecl {
  self: Self =>
}
