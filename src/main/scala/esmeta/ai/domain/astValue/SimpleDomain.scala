package esmeta.ai.domain.astValue

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.state.*

/** simple domain for AST values */
object SimpleDomain extends astValue.Domain with SimpleDomain[AstValue]("AST")
