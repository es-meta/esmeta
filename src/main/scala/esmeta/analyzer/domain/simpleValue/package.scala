package esmeta.analyzer.domain.simpleValue

import esmeta.analyzer.{Analyzer, domain}

type Self = Decl & domain.Decl & Analyzer

trait Decl extends SimpleValueDomainDecl with SimpleValueBasicDomainDecl {
  self: Self =>
}
