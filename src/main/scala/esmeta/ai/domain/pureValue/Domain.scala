package esmeta.ai.domain.pureValue

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.state.*

/** abstract pure value (value except completion record) domain */
trait Domain
  extends domain.Domain[PureValue]
  with Prunable[PureValue]
  with Meetable[PureValue]
