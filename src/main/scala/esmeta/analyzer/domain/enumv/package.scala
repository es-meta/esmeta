package esmeta.analyzer.domain.enumv

import esmeta.analyzer.{Analyzer, domain}

type Self = Decl & domain.Decl & Analyzer

trait Decl
  extends EnumDomainDecl
  with EnumSimpleDomainDecl
  with EnumFlatDomainDecl
  with EnumSetDomainDecl {
  self: Self =>
}
