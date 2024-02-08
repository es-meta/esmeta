package esmeta.analyzer.domain.part

import esmeta.analyzer.{Analyzer, domain}

type Self = Decl & domain.Decl & Analyzer

trait Decl
  extends PartDomainDecl
  with PartSimpleDomainDecl
  with PartFlatDomainDecl
  with PartSetDomainDecl {
  self: Self =>
}
