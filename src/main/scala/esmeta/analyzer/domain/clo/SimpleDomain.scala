package esmeta.analyzer.domain.clo

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

/** simple domain for closure values */
object SimpleDomain extends clo.Domain with SimpleDomain[AClo]("clo")
