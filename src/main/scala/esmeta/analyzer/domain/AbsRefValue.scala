package esmeta.analyzer.domain

import esmeta.analyzer.*
import esmeta.ir.*

trait AbsRefTargetDecl { self: Self =>

  /** basic abstract reference values */
  sealed trait AbsRefTarget:
    override def toString: String = this match
      case AbsRefVar(x)             => s"$x"
      case AbsRefField(base, field) => s"$base[$field]"
  case class AbsRefVar(x: Var) extends AbsRefTarget
  case class AbsRefField(base: AbsValue, field: AbsValue) extends AbsRefTarget
}
