package esmeta.analyzer.domain.cont

import esmeta.analyzer.{Analyzer, domain}

type Self = Decl & domain.Decl & Analyzer

trait Decl
  extends ContDomainDecl
  with ContSimpleDomainDecl
  with ContFlatDomainDecl
  with ContSetDomainDecl {
  self: Self =>
}
