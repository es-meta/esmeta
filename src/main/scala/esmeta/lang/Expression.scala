package esmeta.lang

import esmeta.lang.util.*
import esmeta.util.DoubleEquals

// metalanguage expressions
sealed trait Expression extends Syntax
object Expression extends Parser.From[Expression]

// string concatenation expressions
case class StringConcatExpression(exprs: List[Expression]) extends Expression

// list concatenation expressions
case class ListConcatExpression(exprs: List[Expression]) extends Expression

// record expressions
case class RecordExpression(
  ty: Type,
  fields: List[(FieldLiteral, Expression)],
) extends Expression

// `length of <string>` expressions
case class LengthExpression(expr: Expression) extends Expression with Diverged

// `substring of` expressions
case class SubstringExpression(
  expr: Expression,
  from: Expression,
  to: Expression,
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

// return-if-abrupt expressions
case class ReturnIfAbruptExpression(
  expr: InvokeExpression,
  check: Boolean,
) extends Expression

// list expressions
case class ListExpression(entries: List[Expression]) extends Expression

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
  ty: Type,
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
// metalanguage calcualation expressions
// -----------------------------------------------------------------------------
sealed trait CalcExpression extends Expression

// reference expressions
case class ReferenceExpression(ref: Reference) extends CalcExpression

// mathematical operation expressions
case class MathOpExpression(
  op: MathOpExpression.Op,
  args: List[Expression],
) extends CalcExpression
object MathOpExpression:
  enum Op extends LangElem:
    case Max, Min, Abs, Floor, ToBigInt, ToNumber, ToMath

// exponentiation expressions
case class ExponentiationExpression(
  base: CalcExpression,
  power: CalcExpression,
) extends CalcExpression

// binary expressions
case class BinaryExpression(
  left: CalcExpression,
  op: BinaryExpression.Op,
  right: CalcExpression,
) extends CalcExpression
object BinaryExpression:
  enum Op extends LangElem:
    case Add, Sub, Mul, Div, Mod

// unary expressions
case class UnaryExpression(
  op: UnaryExpression.Op,
  expr: CalcExpression,
) extends CalcExpression
object UnaryExpression:
  enum Op extends LangElem:
    case Neg

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
) extends Literal

// constant literals
case class ConstLiteral(name: String) extends Literal

// string literals
case class StringLiteral(s: String) extends Literal

// field literals
case class FieldLiteral(name: String) extends Literal

// symbol literals
case class SymbolLiteral(sym: String) extends Literal

// numeric literals
sealed trait NumericLiteral extends Literal
sealed trait MathValueLiteral extends NumericLiteral
case class PositiveInfinityMathValueLiteral() extends MathValueLiteral
case class NegativeInfinityMathValueLiteral() extends MathValueLiteral
case class DecimalMathValueLiteral(n: BigDecimal) extends MathValueLiteral
case class NumberLiteral(n: Double) extends NumericLiteral with DoubleEquals(n)
case class BigIntLiteral(n: BigInt) extends NumericLiteral

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
