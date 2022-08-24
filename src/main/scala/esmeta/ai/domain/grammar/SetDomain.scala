package esmeta.ai.domain.grammar

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.state.*

/** set domain for grammar goal symbol values */
class SetDomain(
  maxSizeOpt: Option[Int] = None, // max size of set
) extends grammar.Domain
  with domain.SetDomain[Grammar]("grammar", maxSizeOpt)
