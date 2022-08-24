package esmeta.ai.domain.number

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.state.*

/** simple domain for number values */
object SimpleDomain extends number.Domain with SimpleDomain[Number]("number")
