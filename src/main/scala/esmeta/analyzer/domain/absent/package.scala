package esmeta.analyzer.domain.absent

import esmeta.analyzer.{Analyzer, domain}

type Self = Decl & domain.Decl & Analyzer

trait Decl extends AbsentDomainDecl with AbsentSimpleDomainDecl { self: Self =>
}
