package esmeta.ai.domain.const

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.state.*

/** set domain for constant values */
class SetDomain(
  maxSizeOpt: Option[Int] = None, // max size of set
) extends const.Domain
  with domain.SetDomain[Const]("const", maxSizeOpt)
