package esmeta.analyzer.domain.grammar

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

/** set domain for grammar goal symbol values */
class SetDomain(
  maxSizeOpt: Option[Int] = None, // max size of set
) extends grammar.Domain
  with domain.SetDomain[Grammar]("grammar", maxSizeOpt)
