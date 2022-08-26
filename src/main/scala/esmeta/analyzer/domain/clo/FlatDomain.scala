package esmeta.analyzer.domain.clo

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

/** flat domain for closure values */
object FlatDomain extends clo.Domain with FlatDomain[AClo]("clo")
