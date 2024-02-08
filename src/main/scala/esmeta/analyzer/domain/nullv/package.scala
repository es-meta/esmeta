package esmeta.analyzer.domain.nullv

import esmeta.analyzer.{Analyzer, domain}

type Self = Decl & domain.Decl & Analyzer

trait Decl extends NullDomainDecl with NullSimpleDomainDecl { self: Self => }
