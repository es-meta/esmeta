package esmeta.state

import esmeta.util.Loc

/** Nearest syntax Information */
case class Nearest(prodName: String, rhsIdx: Int, subIdx: Int, loc: Loc)
