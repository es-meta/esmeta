package esmeta.analyzer.domain.bigInt

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

/** flat domain for big integer values */
object FlatDomain extends bigInt.Domain with FlatDomain[BigInt]("bigInt")
