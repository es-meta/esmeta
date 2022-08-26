package esmeta.analyzer.domain.const

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

/** flat domain for constant values */
object FlatDomain extends const.Domain with FlatDomain[Const]("const")
