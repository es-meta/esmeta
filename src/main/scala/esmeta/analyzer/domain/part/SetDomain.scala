package esmeta.analyzer.domain.part

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

/** set domain for address partitions */
class SetDomain(
  maxSizeOpt: Option[Int] = None, // max size of set
) extends part.Domain
  with domain.SetDomain[Part]("part", maxSizeOpt)
