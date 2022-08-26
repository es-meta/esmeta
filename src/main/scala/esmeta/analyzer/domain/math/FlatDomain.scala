package esmeta.analyzer.domain.math

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

/** flat domain for mathematical values */
object FlatDomain extends math.Domain with FlatDomain[Math]("math")
