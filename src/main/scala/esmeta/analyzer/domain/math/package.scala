package esmeta.analyzer.domain.math

import esmeta.analyzer.{Analyzer, domain}

type Self = Decl & domain.Decl & Analyzer

trait Decl
  extends MathDomainDecl
  with MathSimpleDomainDecl
  with MathFlatDomainDecl
  with MathSetDomainDecl {
  self: Self =>
}
