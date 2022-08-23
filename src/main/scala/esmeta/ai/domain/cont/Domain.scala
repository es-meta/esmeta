package esmeta.ai.domain.cont

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.state.Cont

/** abstract continuation domain */
trait Domain extends domain.Domain[Cont] with Prunable[Cont] with Meetable[Cont]
