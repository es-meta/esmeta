package esmeta.analyzer.domain.grammar

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

/** simple domain for grammar goal symbol values */
object SimpleDomain extends grammar.Domain with SimpleDomain[Grammar]("grammar")
