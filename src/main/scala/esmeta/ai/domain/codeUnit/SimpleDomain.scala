package esmeta.ai.domain.codeUnit

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.state.*

/** simple domain for code unit values */
object SimpleDomain
  extends codeUnit.Domain
  with SimpleDomain[CodeUnit]("codeUnit")
