package esmeta.analyzer.domain.bigInt

import esmeta.analyzer.{Analyzer, domain}

type Self = Decl & domain.Decl & Analyzer

trait Decl
  extends BigIntDomainDecl
  with BigIntSimpleDomainDecl
  with BigIntFlatDomainDecl
  with BigIntSetDomainDecl {
  self: Self =>
}
