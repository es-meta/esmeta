package esmeta.analyzer.domain.cont

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

/** simple domain for continuation values */
object SimpleDomain extends cont.Domain with SimpleDomain[ACont]("cont")
