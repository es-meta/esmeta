package esmeta.analyzer.domain.number

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

/** flat domain for number values */
object FlatDomain extends number.Domain with FlatDomain[Number]("number")
