package esmeta.ai.domain.clo

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.state.*

/** set domain for closure values */
class SetDomain(
  maxSizeOpt: Option[Int] = None, // max size of set
) extends clo.Domain
  with domain.SetDomain[AClo]("clo", maxSizeOpt)
