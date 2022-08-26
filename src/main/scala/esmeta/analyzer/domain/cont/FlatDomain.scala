package esmeta.analyzer.domain.cont

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

/** flat domain for continuation values */
object FlatDomain extends cont.Domain with FlatDomain[ACont]("cont")
