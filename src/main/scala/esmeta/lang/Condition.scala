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
  ref: Reference,
  negation: Boolean,
  field: Expression,
) extends Condition

// production condition such as
// `|Declartion| is <emu-grammar>Declaration: HoistableDeclaration</emu-grammar>`
// XXX use production literal?
case class ProductionCondition(
  ntLiteral: Expression,
  lhsName: String,
  rhsName: String,
) extends Condition

// predicate condition
case class PredicateCondition(
  expr: Expression,
  negation: Boolean,
  op: PredicateCondition.Op,
) extends Condition
object PredicateCondition:
  enum Op extends LangElem:
    case Abrupt, NeverAbrupt, Normal, Finite, Duplicated, Present, Empty,
    StrictMode, ArrayIndex, NonNegative, FalseToken, TrueToken, DataProperty,
    AccessorProperty, FullyPopulated, Nonterminal, IntegralNumber

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

// `contains ... whose` conditions
case class ContainsWhoseCondition(
  list: Expression,
  ty: Type,
  fieldName: String,
  expr: Expression,
) extends Condition

// compound conditions
case class CompoundCondition(
  left: Condition,
  op: CompoundCondition.Op,
  right: Condition,
) extends Condition
object CompoundCondition:
  enum Op extends LangElem:
    case And, Or, Imply
