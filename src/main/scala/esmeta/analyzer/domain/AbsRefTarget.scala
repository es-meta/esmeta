package esmeta.analyzer.domain

import esmeta.analyzer.*
import esmeta.ir.*

trait AbsRefTargetDecl { self: Self =>

  /** basic abstract reference values */
  sealed trait AbsRefTarget:
    override def toString: String = this match
      case AbsVarTarget(x)             => s"$x"
      case AbsFieldTarget(base, field) => s"$base[$field]"
  case class AbsVarTarget(x: Var) extends AbsRefTarget
  case class AbsFieldTarget(base: AbsValue, field: AbsValue)
    extends AbsRefTarget
}
