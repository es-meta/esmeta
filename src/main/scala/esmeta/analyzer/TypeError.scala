package esmeta.analyzer

import esmeta.cfg.*
import esmeta.error.{InvalidAnalysisPointMerge, InvalidTypeErrorMerge}
import esmeta.ir.{Func => _, *}
import esmeta.ty.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.analyzer.util.Fingerprintifier

/** type errors in specification */
sealed trait TypeError extends AnalyzerElem {
  type This <: TypeError
  val point: AnalysisPoint with TypeErrorPoint[This]
  inline def func: Func = point.func
  def toFingerprint: String =
    import Fingerprintifier.errorRule
    stringify(this)
  def +(that: TypeError): TypeError = (this, that) match
    case (ParamTypeMismatch(lp, lty), ParamTypeMismatch(rp, rty)) =>
      ParamTypeMismatch(lp + rp, lty || rty)
    case (ReturnTypeMismatch(lp, lty), ReturnTypeMismatch(rp, rty)) =>
      ReturnTypeMismatch(lp + rp, lty || rty)
    case (UncheckedAbruptComp(lp, lty), UncheckedAbruptComp(rp, rty)) =>
      UncheckedAbruptComp(lp + rp, lty || rty)
    case (InvalidPropBase(lp, lty), InvalidPropBase(rp, rty)) =>
      InvalidPropBase(lp + rp, lty || rty)
    case (UnaryOpTypeMismatch(lp, lty), UnaryOpTypeMismatch(rp, rty)) =>
      UnaryOpTypeMismatch(lp + rp, lty || rty)
    case (BinaryOpTypeMismatch(lp, ll, lr), BinaryOpTypeMismatch(rp, rl, rr)) =>
      BinaryOpTypeMismatch(lp + rp, ll || rl, lr || rr)
    case _ => throw InvalidTypeErrorMerge(this, that)
}

/** parameter type mismatches */
case class ParamTypeMismatch(
  point: ArgAssignPoint,
  argTy: ValueTy,
) extends TypeError { type This = ParamTypeMismatch }

/** return type mismatches */
case class ReturnTypeMismatch(
  point: InternalReturnPoint,
  retTy: ValueTy,
) extends TypeError { type This = ReturnTypeMismatch }

/** unchecked abrupt completion */
case class UncheckedAbruptComp(
  point: ReturnIfAbruptPoint,
  ty: ValueTy,
) extends TypeError { type This = UncheckedAbruptComp }

/** invalid base in property referecens */
case class InvalidPropBase(
  point: PropBasePoint,
  baseTy: ValueTy,
) extends TypeError { type This = InvalidPropBase }

/** operand type mismatches for unary operators */
case class UnaryOpTypeMismatch(
  point: UnaryOpPoint,
  operandTy: ValueTy,
) extends TypeError { type This = UnaryOpTypeMismatch }

/** operand type mismatches for binary operators */
case class BinaryOpTypeMismatch(
  point: BinaryOpPoint,
  lhsTy: ValueTy,
  rhsTy: ValueTy,
) extends TypeError { type This = BinaryOpTypeMismatch }
