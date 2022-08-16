package esmeta.state

import esmeta.ir.Id

/** IR reference value */
sealed trait RefValue extends StateElem
case class IdValue(id: Id) extends RefValue
case class PropValue(base: Value, prop: PureValue) extends RefValue
