package esmeta.ai.domain

import esmeta.ir.*

// basic abstract reference values
sealed trait AbsRefValue:
  override def toString: String = this match
    case AbsRefId(id)           => s"$id"
    case AbsRefProp(base, prop) => s"$base[$prop]"
case class AbsRefId(id: Id) extends AbsRefValue
case class AbsRefProp(base: AbsValue, prop: AbsValue) extends AbsRefValue
