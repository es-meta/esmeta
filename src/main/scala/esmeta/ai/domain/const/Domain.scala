package esmeta.ai.domain.const

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.state.Const

/** abstract constant domain */
trait Domain
  extends domain.Domain[Const]
  with Prunable[Const]
  with Meetable[Const]
