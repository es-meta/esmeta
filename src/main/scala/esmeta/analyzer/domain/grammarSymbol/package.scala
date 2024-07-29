package esmeta.analyzer.domain.nt

import esmeta.analyzer.{Analyzer, domain}

type Self = Decl & domain.Decl & Analyzer

trait Decl
  extends GrammarSymbolDomainDecl
  with GrammarSymbolSimpleDomainDecl
  with GrammarSymbolFlatDomainDecl
  with GrammarSymbolSetDomainDecl {
  self: Self =>
}
