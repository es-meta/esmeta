package esmeta.analyzer.domain.undef

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

/** simple domain for undefined values */
object SimpleDomain extends undef.Domain with SimpleDomain("undef", Fin(Undef))
