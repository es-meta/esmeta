package esmeta.ai.domain.nullv

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.state.*

/** simple domain for null values */
object SimpleDomain extends nullv.Domain with SimpleDomain("null", Fin(Null))
