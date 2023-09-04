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
      case ParamTypeMismatch(point, argTy) =>
        app >> point.param.ty
      case ReturnTypeMismatch(point, retTy) =>
        app >> point.returnNp.func.retTy
      case UncheckedAbruptComp(point, ty) => app >> point.riaExpr
      case InvalidPropBase(point, baseTy) =>
        app >> point.propPoint.prop
      case UnaryOpTypeMismatch(point, operandTy) =>
        app >> operandTy
      case BinaryOpTypeMismatch(point, lhsTy, rhsTy) => app
    printStep(app, error)
    printHash(app, error.point.func)

  // analysis points
  given apRule: Rule[AnalysisPoint] = (app, ap) =>
    app >> funcSymbol(ap.func)
    ap match
      case cp: ControlPoint            => app
      case CallPoint(callerNp, callee) => app >> funcSymbol(callee)
      case aap @ ArgAssignPoint(cp, idx) =>
        app >> funcSymbol(
          cp.callee,
        ) >> s"${idx + 1} "
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

  private val addLocRule: Rule[IRElem with LangEdge] = (app, elem) =>
    for {
      lang <- elem.langOpt
      loc <- lang.loc
    } app >> " " >> loc.toString
    app

  /** Print where the point of error is located in the algorithm */
  private def printStep(app: Appender, error: TypeError): Appender =
    given Rule[IRElem with LangEdge] = addLocRule
    error match
      case ParamTypeMismatch(point, argTy) =>
        app >> point.callPoint.callerNp.node.callInst
      case ReturnTypeMismatch(point, retTy)      => app >> point.irReturn
      case UncheckedAbruptComp(point, ty)        => app >> point.riaExpr
      case InvalidPropBase(point, baseTy)        => app >> point.propPoint.prop
      case UnaryOpTypeMismatch(point, operandTy) => app >> point.unary
      case BinaryOpTypeMismatch(point, lhsTy, rhsTy) => app >> point.binary
    app >> " "

  /** Print hash obtained from the algorithm's head and body */
  private def printHash(app: Appender, func: Func): Appender =
    app >> (func.irFunc.algo match
      // this happens for manually written IRs
      case None => "".hashCode().toHexString
      case Some(value) =>
        (value.head.toString() ++ value.body.toString()).hashCode().toHexString
    )
}
