package esmeta.analyzer.domain.pureValue

import esmeta.analyzer.{Analyzer, domain}

type Self = Decl & domain.Decl & Analyzer

trait Decl extends PureValueDomainDecl with PureValueBasicDomainDecl {
  self: Self =>
}
