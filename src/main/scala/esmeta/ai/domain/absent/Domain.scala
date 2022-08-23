package esmeta.ai.domain.absent

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.state.*

/** abstract absent domain */
trait Domain
  extends domain.Domain[Absent]
  with Prunable[Absent]
  with Meetable[Absent]
