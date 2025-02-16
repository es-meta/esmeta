package esmeta.analyzer.util

import esmeta.analyzer.Analyzer

type Self = Decl & Analyzer

trait Decl extends GraphDecl with DotPrinterDecl {
  self: Self =>
}
