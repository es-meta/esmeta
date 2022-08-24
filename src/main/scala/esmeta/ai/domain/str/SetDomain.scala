package esmeta.ai.domain.str

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.state.*

/** set domain for string values */
class SetDomain(
  maxSizeOpt: Option[Int] = None, // max size of set
) extends str.Domain
  with domain.SetDomain[Str]("str", maxSizeOpt)
