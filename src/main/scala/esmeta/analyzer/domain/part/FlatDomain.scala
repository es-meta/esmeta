package esmeta.analyzer.domain.part

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

/** flat domain for address partitions */
object FlatDomain extends part.Domain with FlatDomain[Part]("part")
