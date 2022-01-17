package esmeta.lang

import esmeta.util.DoubleEquals

/** programs for abstract algorithms */
case class Program(block: Block) extends LangElem

// -----------------------------------------------------------------------------
// algorithm blocks
// -----------------------------------------------------------------------------
sealed trait Block extends LangElem
object Block extends Parser[Block]

case class Order(steps: List[Step]) extends Block
case class Unorder(steps: List[Step]) extends Block
case class Figure(lines: List[String]) extends Block

// -----------------------------------------------------------------------------
// algorithm steps
// -----------------------------------------------------------------------------
sealed trait Step extends LangElem
object Step extends Parser[Step]

// let steps
case class LetStep(variable: Variable, expr: Expression) extends Step

// if-then-else steps
// TODO stringifer/parsers for `else`-steps
case class IfStep(cond: Condition, thenStep: Step, elseStep: Option[Step])
  extends Step

// return steps
case class ReturnStep(expr: Expression) extends Step

// assertion steps
case class AssertStep(cond: Condition) extends Step

// for-each steps for integers
case class ForEachIntegerStep(
  variable: Variable,
  start: Expression,
  cond: Condition,
  ascending: Boolean,
  body: Step,
) extends Step

// block steps
case class BlockStep(block: Block) extends Step

// throw steps
case class ThrowStep(errorName: String) extends Step

// perform steps
case class PerformStep(expr: Expression) extends Step

// not yet supported steps
case class YetStep(str: String, block: Option[Block]) extends Step

// -----------------------------------------------------------------------------
// algorithm expressions
// -----------------------------------------------------------------------------
sealed trait Expression extends LangElem
object Expression extends Parser[Expression]

// `length of` expressions
case class LengthExpression(expr: Expression) extends Expression

// `substring of` expressions
case class SubstringExpression(
  expr: Expression,
  from: Expression,
  to: Expression,
) extends Expression

// empty string expressions
case object EmptyStringExpression extends Expression

// calcualation expressions
sealed trait CalcExpression extends Expression

// identifier expressions
case class IdentifierExpression(id: Identifier) extends CalcExpression

// algorithm invocation expressions
case class InvokeExpression(
  name: String,
  args: List[CalcExpression],
) extends CalcExpression

// return if abrupt expressions
case class ReturnIfAbruptExpression(
  expr: CalcExpression,
  check: Boolean,
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
// algorithm literals
// -----------------------------------------------------------------------------
sealed trait Literal extends CalcExpression

// string literals
case class StringLiteral(s: String) extends Literal

// numeric literals
sealed trait NumericLiteral extends Literal
sealed trait MathValueLiteral extends NumericLiteral
case object PositiveInfinityMathValueLiteral extends MathValueLiteral
case object NegativeInfinityMathValueLiteral extends MathValueLiteral
case class DecimalMathValueLiteral(n: BigDecimal) extends MathValueLiteral
case class NumberLiteral(n: Double) extends NumericLiteral with DoubleEquals(n)
case class BigIntLiteral(n: BigInt) extends NumericLiteral

// boolean literals
sealed trait BooleanLiteral extends Literal
case object TrueLiteral extends BooleanLiteral
case object FalseLiteral extends BooleanLiteral

// other special literals
case object UndefinedLiteral extends Literal
case object NullLiteral extends Literal

// -----------------------------------------------------------------------------
// algorithm conditions
// -----------------------------------------------------------------------------
sealed trait Condition extends LangElem
object Condition extends Parser[Condition]

// expression conditions
case class ExpressionCondition(expr: Expression) extends Condition

// binary conditions
case class BinaryCondition(
  left: Expression,
  op: BinaryCondition.Op,
  right: Expression,
) extends Condition
object BinaryCondition:
  enum Op extends LangElem:
    case Is, NIs, Eq, NEq, LessThan, LessThanEqual, GreaterThan,
    GreaterThanEqual, SameCodeUnits

// compound conditions
case class CompoundCondition(
  left: Condition,
  op: CompoundCondition.Op,
  right: Condition,
) extends Condition
object CompoundCondition:
  enum Op extends LangElem:
    case And, Or

// -----------------------------------------------------------------------------
// algorithm identifiers
// -----------------------------------------------------------------------------
sealed trait Identifier extends LangElem
object Identifier extends Parser[Identifier]

case class Variable(name: String) extends Identifier
