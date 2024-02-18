package esmeta.analyzer.domain.infinity

import esmeta.analyzer.{Analyzer, domain}

type Self = Decl & domain.Decl & Analyzer

trait Decl extends InfinityDomainDecl with InfinityFlatDomainDecl {
  self: Self =>
}
