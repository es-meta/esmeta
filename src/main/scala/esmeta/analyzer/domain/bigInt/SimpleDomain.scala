package esmeta.analyzer.domain.bigInt

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

/** simple domain for big integer values */
object SimpleDomain extends bigInt.Domain with SimpleDomain[BigInt]("bigInt")
