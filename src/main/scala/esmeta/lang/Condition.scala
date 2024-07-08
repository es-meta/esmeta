package esmeta.lang

import esmeta.lang.util.*

// metalanguage conditions
sealed trait Condition extends Syntax
object Condition extends Parser.From(Parser.cond)

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

// binding inclusion conditions
case class HasBindingCondition(
  ref: Reference,
  negation: Boolean,
  binding: Expression,
) extends Condition

// production condition such as
// `|Declaration| is <emu-grammar>Declaration: HoistableDeclaration</emu-grammar>`
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
  op: PredicateConditionOperator,
) extends Condition
enum PredicateConditionOperator extends LangElem:
  case Abrupt, Throw, Return, Break, Continue, NeverAbrupt, Normal, Finite,
  Duplicated, Present, Empty, StrictMode, ArrayIndex, FalseToken, TrueToken,
  DataProperty, AccessorProperty, FullyPopulated, Nonterminal

// `A is/are B` conditions
case class IsAreCondition(
  left: List[Expression],
  negation: Boolean,
  right: List[Expression],
) extends Condition

// binary conditions
case class BinaryCondition(
  left: Expression,
  op: BinaryConditionOperator,
  right: Expression,
) extends Condition
enum BinaryConditionOperator extends LangElem:
  case Eq, NEq, LessThan, LessThanEqual, GreaterThan, GreaterThanEqual,
  SameCodeUnits

// inclusive interval conditions
case class InclusiveIntervalCondition(
  left: Expression,
  negation: Boolean,
  from: Expression,
  to: Expression,
) extends Condition

// `contiains` conditions
case class ContainsCondition(
  list: Expression,
  negation: Boolean,
  target: ContainsConditionTarget,
) extends Condition
enum ContainsConditionTarget extends LangElem:
  case Expr(expr: Expression)
  // `contains ... whose [[ ... ]] is ...` condition
  case WhoseField(tyOpt: Option[Type], fieldName: String, expr: Expression)
  // `contains ... such that ...` condition
  case SuchThat(tyOpt: Option[Type], x: Variable, cond: Condition)

// compound conditions
case class CompoundCondition(
  left: Condition,
  op: CompoundConditionOperator,
  right: Condition,
) extends Condition
enum CompoundConditionOperator extends LangElem:
  case And, Or, Imply
