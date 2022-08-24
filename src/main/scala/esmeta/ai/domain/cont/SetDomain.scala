package esmeta.ai.domain.cont

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.state.*

/** set domain for continuation values */
class SetDomain(
  maxSizeOpt: Option[Int] = None, // max size of set
) extends cont.Domain
  with domain.SetDomain[ACont]("cont", maxSizeOpt)
