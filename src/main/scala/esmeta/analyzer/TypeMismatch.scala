package esmeta.analyzer

import esmeta.cfg.*
import esmeta.ir.{Func => _, *}
import esmeta.ty.*
import esmeta.util.*

/** specification type mismatches */
sealed abstract class TypeMismatch(
  val ap: AnalysisPoint,
) extends AnalyzerElem {
  inline def func: Func = ap.func
}

/** parameter type mismatches */
case class ParamTypeMismatch(
  aap: ArgAssignPoint[Node],
  actual: ValueTy,
) extends TypeMismatch(aap)

/** return type mismatches */
case class ReturnTypeMismatch(
  irp: InternalReturnPoint,
  actual: ValueTy,
) extends TypeMismatch(irp)

/** arity mismatches */
case class ArityMismatch(
  cp: CallPoint[Node],
  actual: Int,
) extends TypeMismatch(cp)

/** unchecked abrupt completion mismatches */
case class UncheckedAbruptCompletionMismatch(
  riap: ReturnIfAbruptPoint,
  actual: ValueTy,
) extends TypeMismatch(riap)

/** invalid function call mismatches */
case class InvalidCallMismatch(
  tcp: TryCallPoint,
) extends TypeMismatch(tcp)
