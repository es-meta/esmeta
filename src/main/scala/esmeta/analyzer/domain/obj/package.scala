package esmeta.analyzer.domain.obj

import esmeta.analyzer.{Analyzer, domain}

type Self = Decl & domain.Decl & Analyzer

trait Decl extends ObjDomainDecl with ObjBasicDomainDecl { self: Self => }
