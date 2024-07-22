package esmeta.analyzer.domain

import esmeta.analyzer.*
import esmeta.ir.*

trait AbsRefTargetDecl { self: Self =>

  /** basic abstract reference values */
  sealed trait AbsRefTarget:
    override def toString: String = this match
      case AbsRefVar(x)           => s"$x"
      case AbsRefProp(base, prop) => s"$base[$prop]"
  case class AbsRefVar(x: Var) extends AbsRefTarget
  case class AbsRefProp(base: AbsValue, prop: AbsValue) extends AbsRefTarget
}
