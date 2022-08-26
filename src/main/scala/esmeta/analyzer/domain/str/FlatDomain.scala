package esmeta.analyzer.domain.str

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

/** flat domain for string values */
object FlatDomain extends str.Domain with FlatDomain[Str]("str")
