package esmeta.lang

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

case class LetStep(variable: Variable, expr: Expression) extends Step
case class IfStep(cond: Condition, thenStep: Step, elseStep: Option[Step])
  extends Step
case class ReturnStep(expr: Expression) extends Step
case class AssertStep(cond: Condition) extends Step
case class YetStep(str: String, block: Option[Block]) extends Step

// -----------------------------------------------------------------------------
// algorithm expressions
// -----------------------------------------------------------------------------
sealed trait Expression extends LangElem
object Expression extends Parser[Expression]

case class LengthExpression(expr: Expression) extends Expression
case class IdentifierExpression(id: Identifier) extends Expression

// -----------------------------------------------------------------------------
// algorithm values
// -----------------------------------------------------------------------------
sealed trait Literal extends Expression
case object EmptyStringLiteral extends Literal
case class StringLiteral(s: String) extends Literal
sealed trait NumericLiteral extends Literal
sealed trait MathValueLiteral extends NumericLiteral
case object PositiveInfinityMathValueLiteral extends MathValueLiteral
case object NegativeInfinityMathValueLiteral extends MathValueLiteral
case class DecimalMathValueLiteral(n: BigDecimal) extends MathValueLiteral
case class NumberLiteral(n: Double) extends NumericLiteral
case class BigIntLiteral(n: BigInt) extends NumericLiteral
sealed trait BooleanLiteral extends Literal
case object TrueLiteral extends BooleanLiteral
case object FalseLiteral extends BooleanLiteral
case object UndefinedLiteral extends Literal
case object NullLiteral extends Literal

// -----------------------------------------------------------------------------
// algorithm conditions
// -----------------------------------------------------------------------------
sealed trait Condition extends LangElem
case class ExpressionCondition(expr: Expression) extends Condition
case class BinaryCondition(left: Expression, op: BinaryOp, right: Expression)
  extends Condition
case class CompoundCondition(left: Condition, op: CompoundOp, right: Condition)
  extends Condition

enum BinaryOp:
  case Is, NIs, Eq, NEq, LessThan, LessThanEqual, GreaterThan, GreaterThanEqual

enum CompoundOp:
  case And, Or

// -----------------------------------------------------------------------------
// algorithm identifiers
// -----------------------------------------------------------------------------
sealed trait Identifier extends LangElem
object Identifier extends Parser[Identifier]

case class Variable(name: String) extends Identifier
