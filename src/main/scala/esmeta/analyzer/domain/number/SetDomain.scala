package esmeta.analyzer.domain.number

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

/** set domain for number values */
class SetDomain(
  maxSizeOpt: Option[Int] = None, // max size of set
) extends number.Domain
  with domain.SetDomain[Number]("number", maxSizeOpt)
