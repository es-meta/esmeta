package esmeta.analyzer.domain.const

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

/** simple domain for constant values */
object SimpleDomain extends const.Domain with SimpleDomain[Const]("const")
