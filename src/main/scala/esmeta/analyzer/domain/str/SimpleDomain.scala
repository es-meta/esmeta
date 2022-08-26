package esmeta.analyzer.domain.str

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

/** simple domain for string values */
object SimpleDomain extends str.Domain with SimpleDomain[Str]("str")
