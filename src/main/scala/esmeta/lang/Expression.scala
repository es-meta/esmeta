package esmeta.lang

import esmeta.lang.util.*
import esmeta.util.DoubleEquals

// metalanguage expressions
sealed trait Expression extends Syntax
object Expression extends Parser.From(Parser.expr)

// string concatenation expressions
case class StringConcatExpression(exprs: List[Expression]) extends Expression

// list concatenation expressions
case class ListConcatExpression(exprs: List[Expression]) extends Expression

// list copy expressions
case class ListCopyExpression(expr: Expression) extends Expression

// record expressions
case class RecordExpression(
  tname: String,
  fields: List[(FieldLiteral, Expression)],
) extends Expression

// `length of <string>` expressions
case class LengthExpression(expr: Expression) extends Expression

// `substring of` expressions
case class SubstringExpression(
  expr: Expression,
  from: Expression,
  to: Option[Expression],
) extends Expression

// trim expressions
case class TrimExpression(
  expr: Expression,
  leading: Boolean,
  trailing: Boolean,
) extends Expression

// `the number of elements in <list>` expressions
case class NumberOfExpression(expr: Expression) extends Expression

// intrinsic expressions
case class IntrinsicExpression(intr: Intrinsic) extends Expression

// `source text` expressions
case class SourceTextExpression(
  expr: Expression,
) extends Expression

// covered-by expressions
case class CoveredByExpression(code: Expression, rule: Expression)
  extends Expression

// get items ast expressions
case class GetItemsExpression(nt: Expression, expr: Expression)
  extends Expression

// list expressions
case class ListExpression(entries: List[Expression]) extends Expression

// integer list expressions
case class IntListExpression(
  from: CalcExpression,
  isFromInclusive: Boolean,
  to: CalcExpression,
  isToInclusive: Boolean,
  isAscending: Boolean,
) extends Expression

// emu-xref expressions
case class XRefExpression(kind: XRefExpressionOperator, id: String)
  extends Expression
enum XRefExpressionOperator extends LangElem:
  case Algo, InternalSlots, ParamLength

// the sole element expressions
case class SoleElementExpression(list: Expression) extends Expression

// the code unit expression at a specific index of a string
case class CodeUnitAtExpression(base: Expression, index: Expression)
  extends Expression

// not yet supported expressions
case class YetExpression(str: String, block: Option[Block]) extends Expression

// -----------------------------------------------------------------------------
// metalanguage invocation expressions
// -----------------------------------------------------------------------------
sealed trait InvokeExpression extends Expression

// abstract operation (AO) invocation expressions
case class InvokeAbstractOperationExpression(
  name: String,
  args: List[Expression],
) extends InvokeExpression

// numeric method invocation expression
case class InvokeNumericMethodExpression(
  base: String,
  name: String,
  args: List[Expression],
) extends InvokeExpression

// abstract closure invocation expression
case class InvokeAbstractClosureExpression(
  ref: Variable,
  args: List[Expression],
) extends InvokeExpression

// method invocation expressions
case class InvokeMethodExpression(
  ref: PropertyReference,
  args: List[Expression],
) extends InvokeExpression

// syntax-directed operation (SDO) invocation expressions
case class InvokeSyntaxDirectedOperationExpression(
  base: Expression,
  name: String,
  args: List[Expression],
) extends InvokeExpression

// -----------------------------------------------------------------------------
// metalanguage calculation expressions
// -----------------------------------------------------------------------------
sealed trait CalcExpression extends Expression {
  import BinaryExpressionOperator.*

  /** level of calculation expressions */
  def level: Int = this match
    case BinaryExpression(_, Add | Sub, _)       => 0
    case BinaryExpression(_, Mul | Div | Mod, _) => 1
    case UnaryExpression(_, _)                   => 2
    case _                                       => 3
}
object CalcExpression extends Parser.From(Parser.calcExpr)

// return-if-abrupt expressions
case class ReturnIfAbruptExpression(
  expr: Expression,
  check: Boolean,
) extends CalcExpression

// reference expressions
case class ReferenceExpression(ref: Reference) extends CalcExpression

// mathematical function expressions
case class MathFuncExpression(
  op: MathFuncExpressionOperator,
  args: List[CalcExpression],
) extends CalcExpression
enum MathFuncExpressionOperator extends LangElem:
  case Max, Min, Abs, Floor, Truncate

// exponentiation expressions
case class ExponentiationExpression(
  base: CalcExpression,
  power: CalcExpression,
) extends CalcExpression

// binary expressions
case class BinaryExpression(
  left: CalcExpression,
  op: BinaryExpressionOperator,
  right: CalcExpression,
) extends CalcExpression
enum BinaryExpressionOperator extends LangElem:
  case Add, Sub, Mul, Div, Mod

// unary expressions
case class UnaryExpression(
  op: UnaryExpressionOperator,
  expr: CalcExpression,
) extends CalcExpression
enum UnaryExpressionOperator extends LangElem:
  case Neg

// conversion expressions
case class ConversionExpression(
  op: ConversionExpressionOperator,
  expr: Expression,
) extends CalcExpression
enum ConversionExpressionOperator extends LangElem:
  case ToApproxNumber, ToNumber, ToBigInt, ToMath

// -----------------------------------------------------------------------------
// clamp expressions
// -----------------------------------------------------------------------------
case class ClampExpression(
  target: Expression,
  lower: Expression,
  upper: Expression,
) extends Expression
object ClampExpression extends Parser.From(Parser.clampExpr)

// -----------------------------------------------------------------------------
// mathematical operation expressions
// -----------------------------------------------------------------------------
case class MathOpExpression(
  op: MathOpExpressionOperator,
  args: List[CalcExpression],
) extends Expression
object MathOpExpression extends Parser.From(Parser.mathOpExpr)
enum MathOpExpressionOperator extends LangElem:
  case Neg, Add, Sub, Mul, Pow
  case Expm1, Log10, Log2, Cos, Cbrt, Exp, Cosh, Sinh, Tanh, Acos, Acosh
  case Asinh, Atanh, Asin, Atan2, Atan, Log1p, Log, Sin, Sqrt, Tan

// -----------------------------------------------------------------------------
// bitwise expressions
// -----------------------------------------------------------------------------
case class BitwiseExpression(
  left: Expression,
  op: BitwiseExpressionOperator,
  right: Expression,
) extends Expression
object BitwiseExpression extends Parser.From(Parser.bitwiseExpr)
enum BitwiseExpressionOperator extends LangElem:
  case BAnd, BOr, BXOr

// -----------------------------------------------------------------------------
// metalanguage expressions with multiline
// -----------------------------------------------------------------------------
sealed trait MultilineExpression extends Expression

// abstract closure expressions
case class AbstractClosureExpression(
  params: List[Variable],
  captured: List[Variable],
  body: Step,
) extends MultilineExpression

// -----------------------------------------------------------------------------
// metalanguage literals
// -----------------------------------------------------------------------------
sealed trait Literal extends CalcExpression
object Literal extends Parser.From(Parser.literal)

// `this` literals
case class ThisLiteral() extends Literal

// NewTarget literals
case class NewTargetLiteral() extends Literal

// code unit literals with hexadecimal numbers
case class HexLiteral(hex: Int, name: Option[String]) extends Literal

// code literals
case class CodeLiteral(code: String) extends Literal

// nonterminal literals
case class NonterminalLiteral(
  ordinal: Option[Int],
  name: String,
  flags: List[String],
) extends Literal

// enum literals
case class EnumLiteral(name: String) extends Literal

// string literals
case class StringLiteral(s: String) extends Literal

// field literals
case class FieldLiteral(name: String) extends Literal

// production literals
// XXX need to be generalized?
case class ProductionLiteral(lhs: String, rhs: String) extends Literal

// error object literals
case class ErrorObjectLiteral(name: String) extends Literal

// symbol literals
case class SymbolLiteral(sym: String) extends Literal

// numeric literals
sealed trait NumericLiteral extends Literal
sealed trait MathValueLiteral extends NumericLiteral
case class PositiveInfinityMathValueLiteral() extends MathValueLiteral
case class NegativeInfinityMathValueLiteral() extends MathValueLiteral
case class DecimalMathValueLiteral(n: BigDecimal) extends MathValueLiteral
case class MathConstantLiteral(pre: Int, name: String) extends MathValueLiteral
case class NumberLiteral(double: Double)
  extends NumericLiteral
  with DoubleEquals
case class BigIntLiteral(bigInt: BigInt) extends NumericLiteral

// boolean literals
sealed trait BooleanLiteral extends Literal
case class TrueLiteral() extends BooleanLiteral
case class FalseLiteral() extends BooleanLiteral

// other special literals
case class UndefinedLiteral() extends Literal
case class NullLiteral() extends Literal
case class AbsentLiteral() extends Literal

// ECMAScript type literals
sealed trait ESTypeLiteral extends Literal
case class UndefinedTypeLiteral() extends ESTypeLiteral
case class NullTypeLiteral() extends ESTypeLiteral
case class BooleanTypeLiteral() extends ESTypeLiteral
case class StringTypeLiteral() extends ESTypeLiteral
case class SymbolTypeLiteral() extends ESTypeLiteral
case class NumberTypeLiteral() extends ESTypeLiteral
case class BigIntTypeLiteral() extends ESTypeLiteral
case class ObjectTypeLiteral() extends ESTypeLiteral
