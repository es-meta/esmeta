package esmeta.ai.domain.number

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.state.*

/** abstract number domain */
trait Domain
  extends domain.Domain[Number]
  with Prunable[Number]
  with Meetable[Number]
