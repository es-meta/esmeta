package esmeta.analyzer.domain.grammar

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

/** flat domain for grammar goal symbol values */
object FlatDomain extends grammar.Domain with FlatDomain[Grammar]("grammar")
