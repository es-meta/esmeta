package esmeta.analyzer.domain.const

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

/** set domain for constant values */
class SetDomain(
  maxSizeOpt: Option[Int] = None, // max size of set
) extends const.Domain
  with domain.SetDomain[Const]("const", maxSizeOpt)
