package esmeta.analyzer.domain.nt

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

/** simple domain for nonterminals */
object SimpleDomain extends nt.Domain with SimpleDomain[Nt]("nt")
