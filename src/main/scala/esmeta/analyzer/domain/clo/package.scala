package esmeta.analyzer.domain.clo

import esmeta.analyzer.{Analyzer, domain}

type Self = Decl & domain.Decl & Analyzer

trait Decl
  extends CloDomainDecl
  with CloSimpleDomainDecl
  with CloFlatDomainDecl
  with CloSetDomainDecl {
  self: Self =>
}
