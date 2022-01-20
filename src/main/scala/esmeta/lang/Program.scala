package esmeta.lang

import esmeta.util.DoubleEquals

/** programs for abstract algorithms */
case class Program(block: Block) extends LangElem

// -----------------------------------------------------------------------------
// algorithm blocks
// -----------------------------------------------------------------------------
sealed trait Block extends LangElem
object Block extends Parser.From[Block]

case class StepBlock(steps: List[SubStep]) extends Block
case class ExprBlock(exprs: List[Expression]) extends Block
case class Figure(lines: List[String]) extends Block

// sub-steps with optional id tags
case class SubStep(idTag: Option[String], step: Step) extends LangElem

// -----------------------------------------------------------------------------
// algorithm steps
// -----------------------------------------------------------------------------
sealed trait Step extends LangElem
object Step extends Parser.From[Step]

// let steps
case class LetStep(variable: Variable, expr: Expression) extends Step

// set steps
case class SetStep(ref: Reference, expr: Expression) extends Step

// if-then-else steps
case class IfStep(cond: Condition, thenStep: Step, elseStep: Option[Step])
  extends Step

// return steps
case class ReturnStep(expr: Option[Expression]) extends Step

// assertion steps
case class AssertStep(cond: Condition) extends Step

// for-each steps
case class ForEachStep(
  ty: Option[Type],
  variable: Variable,
  expr: Expression,
  body: Step,
) extends Step

// for-each steps for integers
case class ForEachIntegerStep(
  variable: Variable,
  start: Expression,
  cond: Condition,
  ascending: Boolean,
  body: Step,
) extends Step

// throw steps
case class ThrowStep(errorName: String) extends Step

// perform steps
case class PerformStep(expr: Expression) extends Step

// append steps
case class AppendStep(elem: Expression, ref: Reference) extends Step

// repeat steps
case class RepeatStep(cond: Option[Condition], body: Step) extends Step

// push steps
case class PushStep(context: Reference) extends Step

// note steps
case class NoteStep(note: String) extends Step

// suspend steps
case class SuspendStep(context: Reference) extends Step

// block steps
case class BlockStep(block: Block) extends Step

// not yet supported steps
case class YetStep(expr: YetExpression) extends Step

// -----------------------------------------------------------------------------
// algorithm expressions
// -----------------------------------------------------------------------------
sealed trait Expression extends LangElem
object Expression extends Parser.From[Expression]

// string concatenation expressions
case class StringConcatExpression(exprs: List[Expression]) extends Expression

// list concatenation expressions
case class ListConcatExpression(exprs: List[Expression]) extends Expression

// record expressions
case class RecordExpression(
  ty: Type,
  fields: List[(Field, Expression)],
) extends Expression

// type check expressions
case class TypeCheckExpression(
  expr: Expression,
  negation: Boolean,
  ty: Type,
) extends Expression

// `length of` expressions
case class LengthExpression(expr: Expression) extends Expression

// `substring of` expressions
case class SubstringExpression(
  expr: Expression,
  from: Expression,
  to: Expression,
) extends Expression

// intrinsic expressions
case class IntrinsicExpression(intr: Intrinsic) extends Expression

// `source text` expressions
case class SourceTextExpression(
  expr: Expression,
) extends Expression

// algorithm invocation expressions
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

// return-if-abrupt expressions
case class ReturnIfAbruptExpression(
  expr: Expression,
  check: Boolean,
) extends Expression

// list expressions
case class ListExpression(entries: List[Expression]) extends Expression

// not yet supported expressions
case class YetExpression(str: String, block: Option[Block]) extends Expression

// -----------------------------------------------------------------------------
// algorithm calcualation expressions
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
// algorithm literals
// -----------------------------------------------------------------------------
sealed trait Literal extends CalcExpression

// `this` literals
case object ThisLiteral extends Literal

// NewTarget literals
case object NewTargetLiteral extends Literal

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
case class FieldLiteral(field: Field) extends Literal

// symbol literals
case class SymbolLiteral(sym: String) extends Literal

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
object Condition extends Parser.From[Condition]

// expression conditions
case class ExpressionCondition(expr: Expression) extends Condition

// instance check conditions
case class InstanceOfCondition(
  expr: Expression,
  negation: Boolean,
  ty: Type,
) extends Condition

// field inclusion conditions
case class HasFieldCondition(
  expr: Expression,
  negation: Boolean,
  field: Field,
) extends Condition

// abrupt completion check conditions
case class AbruptCompletionCondition(
  x: Variable,
  negation: Boolean,
) extends Condition

// contains condition
case class ContainsCondition(
  expr: Expression,
  elem: Expression,
) extends Condition

// present condition
case class PresentCondition(
  expr: Expression,
  negation: Boolean,
) extends Condition

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
// algorithm references
// -----------------------------------------------------------------------------
sealed trait Reference extends LangElem
object Reference extends Parser.From[Reference]

// variables
case class Variable(name: String) extends Reference

// the running execution context literals
case object RunningExecutionContext extends Reference

// the current realm record
case object CurrentRealmRecord extends Reference

// references to property
case class PropertyReference(base: Reference, prop: Property) extends Reference

// -----------------------------------------------------------------------------
// algorithm properties
// -----------------------------------------------------------------------------
sealed trait Property extends LangElem
object Property extends Parser.From[Property]

// field property
case class FieldProperty(field: Field) extends Property

// component property
case class ComponentProperty(name: String) extends Property

// index property
case class IndexProperty(index: Expression) extends Property

// -----------------------------------------------------------------------------
// algorithm fields
// -----------------------------------------------------------------------------
sealed trait Field extends LangElem
object Field extends Parser.From[Field]
case class StringField(name: String) extends Field
case class IntrinsicField(intrinsic: Intrinsic) extends Field

// -----------------------------------------------------------------------------
// intrinsics
// -----------------------------------------------------------------------------
case class Intrinsic(base: String, props: List[String]) extends LangElem
object Intrinsic extends Parser.From[Intrinsic]

// -----------------------------------------------------------------------------
// algorithm types
// -----------------------------------------------------------------------------
// TODO more detailed instead of strings
case class Type(name: String) extends LangElem
object Type extends Parser.From[Type]
