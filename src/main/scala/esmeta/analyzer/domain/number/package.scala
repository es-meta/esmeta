package esmeta.analyzer.domain.number

import esmeta.analyzer.{Analyzer, domain}

type Self = Decl & domain.Decl & Analyzer

trait Decl
  extends NumberDomainDecl
  with NumberSimpleDomainDecl
  with NumberFlatDomainDecl
  with NumberSetDomainDecl {
  self: Self =>
}
