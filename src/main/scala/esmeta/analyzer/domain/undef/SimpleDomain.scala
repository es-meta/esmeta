package esmeta.analyzer.domain.undef

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*
import esmeta.util.*

/** simple domain for undefined values */
object SimpleDomain extends undef.Domain with SimpleDomain("undef", Fin(Undef))
