package esmeta.state

import esmeta.ir.Var

/** IR reference target */
sealed trait RefTarget extends StateElem
case class VarTarget(x: Var) extends RefTarget
case class PropTarget(base: Value, prop: PureValue) extends RefTarget
