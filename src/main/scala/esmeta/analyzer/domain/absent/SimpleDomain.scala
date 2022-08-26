package esmeta.analyzer.domain.absent

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

/** simple domain for absent values */
object SimpleDomain
  extends absent.Domain
  with SimpleDomain("absent", Fin(Absent))
