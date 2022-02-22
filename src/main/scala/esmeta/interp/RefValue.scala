package esmeta.interp

import esmeta.ir.Id

/** IR reference value */
sealed trait RefValue extends InterpElem
case class IdValue(id: Id) extends RefValue
case class PropValue(base: Value, prop: PureValue) extends RefValue
