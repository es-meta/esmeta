package esmeta.analyzer.domain.part

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

/** simple domain for address partitions */
object SimpleDomain extends part.Domain with SimpleDomain[Part]("part")
