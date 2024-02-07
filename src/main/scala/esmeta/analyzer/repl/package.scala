package esmeta.analyzer.repl

import esmeta.analyzer.Analyzer

type Self = Decl & Analyzer

trait Decl extends ReplDecl with command.Decl { self: Self => }
