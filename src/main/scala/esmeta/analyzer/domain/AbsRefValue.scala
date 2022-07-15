package esmeta.analyzer.domain

import esmeta.analyzer.AnalyzerElem
import esmeta.ir.*

// basic abstract reference values
sealed trait AbsRefValue extends AnalyzerElem
case class AbsRefId(id: Id) extends AbsRefValue
case class AbsRefProp(base: AbsValue, prop: AbsValue) extends AbsRefValue
