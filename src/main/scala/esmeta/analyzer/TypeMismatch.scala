package esmeta.analyzer

import esmeta.cfg.*
import esmeta.ir.{Func => _, *}
import esmeta.ty.*
import esmeta.util.*

/** specification type mismatches */
sealed trait TypeMismatch extends AnalyzerElem

/** parameter type mismatches */
case class ParamTypeMismatch(
  callerNp: NodePoint[Call],
  calleeRp: ReturnPoint,
  param: Param,
  argTy: ValueTy,
) extends TypeMismatch

/** arity mismatches */
case class ArityMismatch(
  callerNp: NodePoint[Call],
  calleeRp: ReturnPoint,
  expected: (Int, Int),
  actual: Int,
) extends TypeMismatch
