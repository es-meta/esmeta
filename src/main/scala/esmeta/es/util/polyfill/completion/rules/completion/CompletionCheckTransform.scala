package esmeta.es.util.polyfill.completion.rules.completion

import esmeta.es.util.polyfill.*
import esmeta.es.util.polyfill.completion.*
import esmeta.es.util.polyfill.completion.CompletionType.*
import esmeta.es.util.polyfill.completion.Eraser.*
import esmeta.es.util.polyfill.PolyfillStep.*
import esmeta.lang.*
import esmeta.lang.BinaryConditionOperator.Eq

/** rewrite a completion check produced by [[CompletionCheckRule]]
  *
  * The condition is rebased onto the completion flag of the target variable,
  * and each branch is rewritten under the completion type that branch implies.
  * When rebasing leaves the condition vacuous, only the `then` branch survives.
  */
object CompletionCheckTransform extends EraseRule {
  def apply(
    step: PolyfillStep,
    config: Config,
    rewriter: Rewriter,
  ): Option[Config] =
    step match {
      case CompletionCheck(
            targetVar,
            checkType,
            flagVar,
            cond,
            thenStep,
            elseStep,
            elseConfig,
          ) =>
        val env = config.env
        val thenType = if (checkType == MayAbrupt) MayAbrupt else MayNormal
        val elseType = if (checkType == MayAbrupt) MayNormal else MayAbrupt

        val Config(thenOptEnv, thenSteps) =
          rewriter.transform(thenStep, Config(env + (targetVar -> thenType)))
        val newThen = thenSteps.toSteps
        val (newElse, elseOptEnv) = elseStep match {
          case Some(e) =>
            val Config(eEnv, steps) =
              rewriter.transform(e, Config(env + (targetVar -> elseType)))
            (Some(steps.toSteps), eEnv)
          case None => (None, env + (targetVar -> elseType))
        }
        val mergedEnv =
          (isTerminal(thenStep), elseStep.map(isTerminal)) match {
            case (true, Some(false)) => elseOptEnv
            case (false, Some(true)) => thenOptEnv
            case _                   => thenOptEnv ++ elseOptEnv
          }

        val flagCheck = BinaryCondition(
          ReferenceExpression(Variable(flagVar, None)),
          Eq,
          if (checkType == MayAbrupt) EnumLiteral("abrupt")
          else EnumLiteral("normal"),
        )
        Some(
          rebaseCondition(cond, Map(targetVar -> flagCheck)) match {
            case Some(newCond) =>
              config(mergedEnv) :+ Branch(
                newCond,
                newThen,
                newElse,
                elseConfig,
              )
            // TODO Can we ignore ElseStep? If not, how can we handle it?
            case None => config(mergedEnv) :+ newThen
          },
        )
      case _ => None
    }
}
