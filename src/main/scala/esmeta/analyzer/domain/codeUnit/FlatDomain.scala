package esmeta.analyzer.domain.codeUnit

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

/** flat domain for code unit values */
object FlatDomain extends codeUnit.Domain with FlatDomain[CodeUnit]("codeUnit")
