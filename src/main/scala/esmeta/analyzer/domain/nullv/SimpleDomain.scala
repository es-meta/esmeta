package esmeta.analyzer.domain.nullv

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

/** simple domain for null values */
object SimpleDomain extends nullv.Domain with SimpleDomain("null", Fin(Null))
