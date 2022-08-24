package esmeta.ai.domain.const

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.state.*

/** flat domain for constant values */
object FlatDomain extends const.Domain with FlatDomain[Const]("const")
