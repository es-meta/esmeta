package esmeta.analyzer.domain.number

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

/** simple domain for number values */
object SimpleDomain extends number.Domain with SimpleDomain[Number]("number")
