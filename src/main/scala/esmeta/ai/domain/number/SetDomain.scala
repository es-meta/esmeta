package esmeta.ai.domain.number

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.state.*

/** set domain for number values */
class SetDomain(
  maxSizeOpt: Option[Int] = None, // max size of set
) extends number.Domain
  with domain.SetDomain[Number]("number", maxSizeOpt)
