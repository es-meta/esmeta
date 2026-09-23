package esmeta.es.util.polyfill.dsl

import esmeta.lang.*

sealed trait Rule[T <: LangElem] {
  def name: String
  def pattern: T
  def replace: Option[T]
  def predicates: Map[String, LangElemPredicate]
  def subrules: List[Rule[LangElem]]

  def isMultiStepRule: Boolean =
    pattern match {
      case _: BlockStep => true
      case _            => false
    }
}

case class StepRule(
  name: String,
  pattern: Step,
  replace: Option[Step],
  predicates: Map[String, LangElemPredicate] = Map.empty,
  subrules: List[Rule[LangElem]] = List.empty,
) extends Rule

case class ExpressionRule(
  name: String,
  pattern: Expression,
  replace: Option[Expression],
  predicates: Map[String, LangElemPredicate] = Map.empty,
  subrules: List[Rule[LangElem]] = List.empty,
) extends Rule

/** Condition-level rule. */
case class ConditionRule(
  name: String,
  pattern: Condition,
  replace: Option[Condition],
  predicates: Map[String, LangElemPredicate] = Map.empty,
  subrules: List[Rule[LangElem]] = List.empty,
) extends Rule

/** Reference-level rule. */
case class ReferenceRule(
  name: String,
  pattern: Reference,
  replace: Option[Reference],
  predicates: Map[String, LangElemPredicate] = Map.empty,
  subrules: List[Rule[LangElem]] = List.empty,
) extends Rule
