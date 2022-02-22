package esmeta.spec

import esmeta.spec.util.*

/** left-hand-sides (LHSs) of productions */
case class Lhs(name: String, params: List[String]) extends SpecElem
object Lhs extends Parser.From[Lhs]
