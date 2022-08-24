package esmeta.ai.domain

import esmeta.ir.*

// basic abstract reference values
sealed trait AbsRefValue
case class AbsRefId(id: Id) extends AbsRefValue
case class AbsRefProp(base: AbsValue, prop: AbsValue) extends AbsRefValue
