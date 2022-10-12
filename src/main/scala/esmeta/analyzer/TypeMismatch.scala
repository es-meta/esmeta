package esmeta.analyzer

import esmeta.cfg.*
import esmeta.ir.{Func => _, *}
import esmeta.ty.*
import esmeta.util.*

/** specification type mismatches */
sealed trait TypeMismatch extends AnalyzerElem {
  def calleeRp: ReturnPoint
  def name: String = calleeRp.func.name
}

/** parameter type mismatches */
case class ParamTypeMismatch(
  callerNp: NodePoint[Call],
  calleeRp: ReturnPoint,
  idx: Int,
  param: Param,
  argTy: ValueTy,
) extends TypeMismatch

/** return type mismatches */
case class ReturnTypeMismatch(
  ret: Return,
  calleeRp: ReturnPoint,
  retTy: ValueTy,
) extends TypeMismatch

/** arity mismatches */
case class ArityMismatch(
  callerNp: NodePoint[Call],
  calleeRp: ReturnPoint,
  expected: (Int, Int),
  actual: Int,
) extends TypeMismatch
