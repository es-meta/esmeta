package esmeta.analyzer.domain

import esmeta.analyzer.*
import esmeta.ir.*

// basic abstract reference values
sealed trait AbsRefValue:
  override def toString: String = this match
    case AbsRefId(id)              => s"$id"
    case AbsRefProp(base, prop, _) => s"$base[$prop]"
case class AbsRefId(id: Id) extends AbsRefValue
// FIXME: support nested baseRef?
case class AbsRefProp(base: AbsValue, prop: AbsValue, id: Option[Id] = None)
  extends AbsRefValue
