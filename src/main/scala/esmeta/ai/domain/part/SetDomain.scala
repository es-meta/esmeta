package esmeta.ai.domain.part

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.state.*

/** set domain for address partitions */
class SetDomain(
  maxSizeOpt: Option[Int] = None, // max size of set
) extends part.Domain
  with domain.SetDomain[Part]("part", maxSizeOpt)
