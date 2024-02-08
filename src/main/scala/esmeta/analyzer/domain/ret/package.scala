package esmeta.analyzer.domain.ret

import esmeta.analyzer.{Analyzer, domain}

type Self = Decl & domain.Decl & Analyzer

trait Decl
  extends RetDomainDecl
  with RetBasicDomainDecl
  with RetTypeDomainDecl {
  self: Self =>
}
