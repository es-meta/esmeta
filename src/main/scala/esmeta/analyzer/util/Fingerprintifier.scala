package esmeta.analyzer.util

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.cfg.*
import esmeta.ir.{IRElem, LangEdge}
import esmeta.state.*
import esmeta.ty.*
import esmeta.ty.util.{Stringifier => TyStringifier}
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*

object Fingerprintifier {
  private val cfgStringifier = CFGElem.getStringifier(false, false)
  import cfgStringifier.given

  private val irStringifier = IRElem.getStringifier(false, false)
  import irStringifier.given

  import TyStringifier.given

  // type errors in specifications
  given errorRule: Rule[TypeError] = (app, error) =>
    app >> errSymbol(error) >> error.point
    error match
      case ParamTypeMismatch(point, argTy)  => app >> point.param.ty
      case ReturnTypeMismatch(point, retTy) => app >> point.returnNp.func.retTy
      case UncheckedAbruptComp(point, ty)   => app >> ty
      case InvalidPropBase(point, baseTy)   => app >> baseTy
      case UnaryOpTypeMismatch(point, operandTy)     => app >> operandTy
      case BinaryOpTypeMismatch(point, lhsTy, rhsTy) => app >> lhsTy >> rhsTy

  // analysis points
  given apRule: Rule[AnalysisPoint] = (app, ap) =>
    app >> apSymbol(ap) >> funcSymbol(ap.func)
    ap match
      case cp: ControlPoint            => app
      case CallPoint(callerNp, callee) => app >> funcSymbol(callee)
      case aap @ ArgAssignPoint(cp, idx) =>
        app >> cp >> idx + 1 >> " " >> aap.param.lhs.name
      case InternalReturnPoint(calleeRp, irReturn) => app
      case ReturnIfAbruptPoint(cp, riaExpr)        => app
      case PropBasePoint(propPoint)                => app
      case PropPoint(cp, prop)                     => app
      case UnaryOpPoint(cp, unary)                 => app >> unary.uop
      case BinaryOpPoint(cp, binary)               => app >> binary.bop

  def apSymbol(ap: AnalysisPoint): String = ap match
    case NodePoint(_, _, _)     => "NP "
    case ReturnPoint(_, _)      => "RP "
    case _: CallPoint           => "CP "
    case _: ArgAssignPoint      => "AAP "
    case _: InternalReturnPoint => "IRP "
    case _: ReturnIfAbruptPoint => "RIAP "
    case _: PropBasePoint       => "PBP "
    case _: PropPoint           => "PP "
    case _: UnaryOpPoint        => "UOP "
    case _: BinaryOpPoint       => "BOP "

  def errSymbol(err: TypeError): String = err match
    case _: ParamTypeMismatch    => "PTM "
    case _: ReturnTypeMismatch   => "RTM "
    case _: UncheckedAbruptComp  => "UAC "
    case _: InvalidPropBase      => "IPB "
    case _: UnaryOpTypeMismatch  => "UOTM "
    case _: BinaryOpTypeMismatch => "BOTM "

  def funcSymbol(func: Func): String = func.irFunc.name + " "
}
