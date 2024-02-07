package esmeta.analyzer.domain.undef

import esmeta.analyzer.{Analyzer, domain}

type Self = Decl & domain.Decl & Analyzer

trait Decl extends UndefDomainDecl with UndefSimpleDomainDecl { self: Self => }
