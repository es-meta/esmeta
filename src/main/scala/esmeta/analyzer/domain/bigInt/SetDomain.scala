package esmeta.analyzer.domain.bigInt

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

/** set domain for big integer values */
class SetDomain(
  maxSizeOpt: Option[Int] = None, // max size of set
) extends bigInt.Domain
  with domain.SetDomain[BigInt]("bigInt", maxSizeOpt)
