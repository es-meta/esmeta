package esmeta.analyzer.domain.nt

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

/** flat domain for nonterminals */
object FlatDomain extends nt.Domain with FlatDomain[Nt]("nt")
