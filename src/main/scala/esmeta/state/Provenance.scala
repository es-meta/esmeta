package esmeta.state

import esmeta.cfg.*

/** provenance of addresses */
case class Provenance(
  cursor: Cursor,
  feature: Option[Feature],
) extends StateElem
