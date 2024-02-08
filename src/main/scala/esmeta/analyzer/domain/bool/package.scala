package esmeta.analyzer.domain.bool

import esmeta.analyzer.{Analyzer, domain}

type Self = Decl & domain.Decl & Analyzer

trait Decl extends BoolDomainDecl with BoolFlatDomainDecl { self: Self => }
