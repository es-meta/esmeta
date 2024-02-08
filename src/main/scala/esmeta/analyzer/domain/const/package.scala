package esmeta.analyzer.domain.const

import esmeta.analyzer.{Analyzer, domain}

type Self = Decl & domain.Decl & Analyzer

trait Decl
  extends ConstDomainDecl
  with ConstSimpleDomainDecl
  with ConstFlatDomainDecl
  with ConstSetDomainDecl {
  self: Self =>
}
