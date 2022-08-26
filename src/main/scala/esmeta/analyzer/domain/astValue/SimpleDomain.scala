package esmeta.analyzer.domain.astValue

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

/** simple domain for AST values */
object SimpleDomain extends astValue.Domain with SimpleDomain[AstValue]("AST")
