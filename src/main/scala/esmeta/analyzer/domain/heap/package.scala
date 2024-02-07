package esmeta.analyzer.domain.heap

import esmeta.analyzer.{Analyzer, domain}

type Self = Decl & domain.Decl & Analyzer

trait Decl extends HeapDomainDecl with HeapBasicDomainDecl { self: Self => }
