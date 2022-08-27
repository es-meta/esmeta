package esmeta.analyzer.domain.absent

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*
import esmeta.util.*

/** simple domain for absent values */
object SimpleDomain
  extends absent.Domain
  with SimpleDomain("absent", Fin(Absent))
