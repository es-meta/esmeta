package esmeta.ai.domain.str

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.state.*

/** flat domain for string values */
object FlatDomain extends str.Domain with FlatDomain[Str]("str")
