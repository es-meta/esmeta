package esmeta.ai.domain.clo

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.state.*

/** flat domain for closure values */
object FlatDomain extends clo.Domain with FlatDomain[AClo]("clo")
