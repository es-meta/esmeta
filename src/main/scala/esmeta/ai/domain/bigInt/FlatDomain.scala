package esmeta.ai.domain.bigInt

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.state.*

/** flat domain for big integer values */
object FlatDomain extends bigInt.Domain with FlatDomain[BigInt]("bigInt")
