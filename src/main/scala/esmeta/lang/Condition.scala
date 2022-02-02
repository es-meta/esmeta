package esmeta.lang

import esmeta.lang.util.*

// metalanguage conditions
sealed trait Condition extends Syntax
object Condition extends Parser.From[Condition]

// expression conditions
case class ExpressionCondition(expr: Expression) extends Condition

// instance check conditions
case class InstanceOfCondition(
  expr: Expression,
  negation: Boolean,
  ty: List[Type],
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

// `A is/are B` conditions
case class IsAreCondition(
  left: List[Expression],
  negation: Boolean,
  right: List[Expression],
) extends Condition

// binary conditions
case class BinaryCondition(
  left: Expression,
  op: BinaryCondition.Op,
  right: Expression,
) extends Condition
object BinaryCondition:
  enum Op extends LangElem:
    case Eq, NEq, LessThan, LessThanEqual, GreaterThan, GreaterThanEqual,
    SameCodeUnits, Contains, NContains

// compound conditions
case class CompoundCondition(
  left: Condition,
  op: CompoundCondition.Op,
  right: Condition,
) extends Condition
object CompoundCondition:
  enum Op extends LangElem:
    case And, Or, Imply
