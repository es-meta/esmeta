package esmeta.es.util.polyfill.analysis

import esmeta.es.util.polyfill.*
import esmeta.es.util.polyfill.CompletionType.*
import esmeta.lang.*
import esmeta.lang.PredicateConditionOperator.*
import esmeta.util.BaseUtils.*

/** an `if` step checking the completion type of a variable, e.g. `if x is an
  * abrupt completion, ...`
  *
  * It extracts the checked completion type and the name of the checked
  * variable.
  */
object CompletionCheckPattern {
  def unapply(step: Step): Option[(CompletionType, String)] = step match {
    case IfStep(cond, _, _, _) => traverseCondition(cond)
    case _                     => None
  }

  private def traverseCondition(
    cond: Condition,
  ): Option[(CompletionType, String)] =
    cond match {
      case PredicateCondition(expr, _, op) =>
        op match {
          case Abrupt | Throw => Some((MayAbrupt, extractVarName(expr)))
          case Normal         => Some((MayNormal, extractVarName(expr)))
          case _              => None
        }
      case CompoundCondition(left, op, right) =>
        traverseCondition(left).orElse(traverseCondition(right))
      case _ => None
    }

  private def extractVarName(expr: Expression) = expr match {
    case ReferenceExpression(Variable(x, _, _, _)) => x
    case err =>
      raise(
        s"Expected Reference Expression for extractVarName, but got '${err.toString}'",
      )
  }
}