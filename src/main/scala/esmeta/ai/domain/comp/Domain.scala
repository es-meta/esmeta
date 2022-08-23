package esmeta.ai.domain.comp

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.state.*

/** abstract completion record domain */
trait Domain extends domain.Domain[Comp] with Prunable[Comp] with Meetable[Comp]
