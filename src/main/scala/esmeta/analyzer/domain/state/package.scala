package esmeta.analyzer.domain.state

import esmeta.analyzer.{Analyzer, domain}

type Self = Decl & domain.Decl & Analyzer

trait Decl
  extends StateDomainDecl
  with StateBasicDomainDecl
  with StateTypeDomainDecl {
  self: Self =>
}
