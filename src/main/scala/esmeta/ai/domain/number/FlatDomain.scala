package esmeta.ai.domain.number

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.state.*

/** flat domain for number values */
object FlatDomain extends number.Domain with FlatDomain[Number]("number")
