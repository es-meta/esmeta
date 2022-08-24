package esmeta.ai.domain.cont

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.state.*

/** flat domain for continuation values */
object FlatDomain extends cont.Domain with FlatDomain[ACont]("cont")
