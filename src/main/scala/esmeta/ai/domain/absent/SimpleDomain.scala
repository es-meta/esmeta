package esmeta.ai.domain.absent

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.state.*

/** simple domain for absent values */
object SimpleDomain
  extends absent.Domain
  with SimpleDomain("absent", Fin(Absent))
