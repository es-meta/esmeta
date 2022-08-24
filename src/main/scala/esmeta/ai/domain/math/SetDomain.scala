package esmeta.ai.domain.math

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.state.*

/** set domain for mathematical values */
class SetDomain(
  maxSizeOpt: Option[Int] = None, // max size of set
) extends math.Domain
  with domain.SetDomain[Math]("math", maxSizeOpt)
