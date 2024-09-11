package esmeta.state

import esmeta.ir.Var

/** IR reference target */
sealed trait RefTarget extends StateElem
case class VarTarget(x: Var) extends RefTarget
case class FieldTarget(base: Value, field: Value) extends RefTarget
