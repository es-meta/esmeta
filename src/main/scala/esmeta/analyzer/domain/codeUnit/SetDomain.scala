package esmeta.analyzer.domain.codeUnit

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

/** set domain for code unit values */
class SetDomain(
  maxSizeOpt: Option[Int] = None, // max size of set
) extends codeUnit.Domain
  with domain.SetDomain[CodeUnit]("codeUnit", maxSizeOpt)
