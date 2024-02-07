package esmeta.analyzer.domain.nt

import esmeta.analyzer.{Analyzer, domain}

type Self = Decl & domain.Decl & Analyzer

trait Decl
  extends NtDomainDecl
  with NtSimpleDomainDecl
  with NtFlatDomainDecl
  with NtSetDomainDecl {
  self: Self =>
}
