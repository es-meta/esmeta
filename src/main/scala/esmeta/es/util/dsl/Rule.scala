package esmeta.es.util.dsl

import esmeta.lang.*

sealed trait Rule[T <: LangElem] {
  def name: String
  def pattern: T
  def replace: Option[T]
  def predicates: Map[String, LangElemPredicate]
  def subrules: List[Rule[LangElem]]

  /** Pretty-print the rule tree for debugging. */
  def prettyPrint(indent: Int = 0): String =
    val pad = "  " * indent
    val kind = this.getClass.getSimpleName
    val predStr =
      if (predicates.isEmpty) ""
      else predicates.keys.mkString("  preds=[", ", ", "]")
    val replStr = replace match
      case Some(_) => "  replace=yes"
      case None    => "  replace=no"
    val header = s"${pad}${kind}(${name})${predStr}${replStr}\n"
    val patStr = s"${pad}  pattern: ${pattern}\n"
    val repStr = replace match
      case Some(r) => s"${pad}  replace: ${r}\n"
      case None    => ""
    val subStr = subrules.map(_.prettyPrint(indent + 1)).mkString
    header + patStr + repStr + subStr

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
