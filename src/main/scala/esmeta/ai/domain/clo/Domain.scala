package esmeta.ai.domain.clo

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.state.Clo

/** abstract closure domain */
trait Domain extends domain.Domain[Clo] with Prunable[Clo] with Meetable[Clo]
