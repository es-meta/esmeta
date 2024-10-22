package esmeta.peval

import esmeta.error.{NonLiteral, PartialEvaluatorError}
import esmeta.ir.*
import esmeta.state.*
import scala.util.{Try}

extension (pv: Predict[Value]) {
  def isKnownLiteral: Boolean = pv match
    case Known(v) => v.isLiteralValue
    case _        => false
}

extension (v: Value) {

  def toExpr: Expr = v match
    case Math(decimal)  => EMath(decimal)
    case Infinity(pos)  => EInfinity(pos)
    case Enum(name)     => EEnum(name)
    case CodeUnit(c)    => ECodeUnit(c)
    case Number(double) => ENumber(double)
    case BigInt(bigInt) => EBigInt(bigInt)
    case Str(str)       => EStr(str)
    case Bool(bool)     => EBool(bool)
    case Undef          => EUndef()
    case Null           => ENull()
    case _              => throw new NonLiteral(v)

  def toExprOpt: Option[Expr] = Try(v.toExpr).toOption

  def isLiteralValue: Boolean = v match
    case Math(_) | Infinity(_) | Enum(_) | CodeUnit(_) | Number(_) | BigInt(_) |
        Str(_) | Bool(_) | Undef | Null =>
      true
    case _ => false
}

extension (sc: StringContext) {
  def throwPeval(args: Any*): Nothing = throw new PartialEvaluatorError(
    sc.s(args),
  )
}

lazy val FORMAL_PARAMS = "FormalParameters"
lazy val ECMASCRIPT_CODE = "ECMAScriptCode"
lazy val FUNC_DECL = "FunctionDeclaration"
lazy val FUNC_DECL_INSTANT = "FunctionDeclarationInstantiation"
lazy val FUNC_BODY = "FunctionBody"
