package esmeta.ai.domain.part

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.state.*

/** flat domain for address partitions */
object FlatDomain extends part.Domain with FlatDomain[Part]("part")
