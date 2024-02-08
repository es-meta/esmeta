package esmeta.analyzer.domain.comp

import esmeta.analyzer.{Analyzer, domain}

type Self = Decl & domain.Decl & Analyzer

trait Decl extends CompDomainDecl with CompBasicDomainDecl { self: Self => }
