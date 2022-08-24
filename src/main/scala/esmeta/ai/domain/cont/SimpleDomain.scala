package esmeta.ai.domain.cont

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.state.*

/** simple domain for continuation values */
object SimpleDomain extends cont.Domain with SimpleDomain[ACont]("cont")
