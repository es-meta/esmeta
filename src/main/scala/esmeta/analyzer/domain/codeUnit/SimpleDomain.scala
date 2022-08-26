package esmeta.analyzer.domain.codeUnit

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

/** simple domain for code unit values */
object SimpleDomain
  extends codeUnit.Domain
  with SimpleDomain[CodeUnit]("codeUnit")
