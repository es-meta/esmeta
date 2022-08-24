package esmeta.ai.domain.astValue

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.state.*

/** set domain for AST values */
class SetDomain(
  maxSizeOpt: Option[Int] = None, // max size of set
) extends astValue.Domain
  with domain.SetDomain[AstValue]("AST", maxSizeOpt)
