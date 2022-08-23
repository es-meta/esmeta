package esmeta.ai.domain.undef

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.state.*

/** abstract undefined domain */
trait Domain
  extends domain.Domain[Undef]
  with Prunable[Undef]
  with Meetable[Undef]
