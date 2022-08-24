package esmeta.ai.domain.undef

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.state.*

/** simple domain for undefined values */
object SimpleDomain extends undef.Domain with SimpleDomain("undef", Fin(Undef))
