package esmeta.analyzer.domain.codeUnit

import esmeta.analyzer.{Analyzer, domain}

type Self = Decl & domain.Decl & Analyzer

trait Decl
  extends CodeUnitDomainDecl
  with CodeUnitSimpleDomainDecl
  with CodeUnitFlatDomainDecl
  with CodeUnitSetDomainDecl {
  self: Self =>
}
