package esmeta.ai.domain.bigInt

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.state.*

/** simple domain for big integer values */
object SimpleDomain extends bigInt.Domain with SimpleDomain[BigInt]("bigInt")
