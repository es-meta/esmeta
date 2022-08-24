package esmeta.ai.domain.bigInt

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.state.*

/** set domain for big integer values */
class SetDomain(
  maxSizeOpt: Option[Int] = None, // max size of set
) extends bigInt.Domain
  with domain.SetDomain[BigInt]("bigInt", maxSizeOpt)
