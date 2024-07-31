package esmeta.analyzer.domain.uninit

import esmeta.analyzer.{Analyzer, domain}

type Self = Decl & domain.Decl & Analyzer

trait Decl extends UninitDomainDecl with UninitSimpleDomainDecl { self: Self =>
}
