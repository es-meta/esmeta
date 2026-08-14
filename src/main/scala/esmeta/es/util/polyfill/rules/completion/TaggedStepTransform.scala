package esmeta.es.util.polyfill.rules.completion

import esmeta.es.util.polyfill.*
import esmeta.es.util.polyfill.CompletionType.*
import esmeta.es.util.polyfill.PolyfillInspector.*
import esmeta.lang.*
import esmeta.lang.BinaryConditionOperator.Eq

/** rewrite a tagged step
  *
  * A tagged `if` step carrying `TARGET_VAR` and `TYPE` is a completion check
  * tagged by [[CompletionCheckRule]]: its condition is rebased on the completion
  * flag of the target variable, and each branch is rewritten with the completion
  * type the branch implies. Other tagged `if` steps only have their branches
  * rewritten, and any other tagged step is rewritten as its inner step.
  */
object TaggedStepTransform extends StepRule {
  def apply(step: Step, config: Config, rewriter: Rewriter): Option[Config] =
    step match {
      case TaggedStep(taggedInnerStep, tag) =>
        taggedInnerStep match {
          case IfStep(cond, thenStep, elseStep, cfg) =>
            val targetVarOpt = tag.get("TARGET_VAR")
            val checkTypeOpt = tag.get("TYPE").map(CompletionType.fromTag)

            (targetVarOpt, checkTypeOpt) match {
              case (Some(targetVar), Some(checkType)) =>
                Some(
                  handleTaggedCompletion(
                    cond,
                    thenStep,
                    elseStep,
                    cfg,
                    tag,
                    targetVar,
                    checkType,
                    config,
                    rewriter,
                  ),
                )
              case _ =>
                Some(
                  handleTaggedGeneric(
                    cond,
                    thenStep,
                    elseStep,
                    cfg,
                    tag,
                    config,
                    rewriter,
                  ),
                )
            }
          case _ => Some(rewriter.transform(taggedInnerStep, config))
        }
      case _ => None
    }

  private def handleTaggedCompletion(
    cond: Condition,
    thenStep: Step,
    elseStep: Option[Step],
    cfg: IfStep.ElseConfig,
    tag: Map[String, String],
    targetVar: String,
    checkType: CompletionType,
    config: Config,
    rewriter: Rewriter,
  ): Config = {
    val env = config.env
    val thenType =
      if (checkType == MayAbrupt) MayAbrupt
      else MayNormal
    val elseType =
      if (checkType == MayAbrupt) MayNormal
      else MayAbrupt

    val thenEnv = env + (targetVar -> thenType)
    val elseEnv = env + (targetVar -> elseType)

    val Config(thenOptEnv, thenSteps) =
      rewriter.transform(thenStep, Config(thenEnv))
    val newThen = thenSteps.toBlockStep
    val (newElse, elseOptEnv) = elseStep match {
      case Some(e) =>
        val Config(eEnv, steps) = rewriter.transform(e, Config(elseEnv))
        (Some(steps.toBlockStep), eEnv)
      case None => (None, elseEnv)
    }
    val mergedEnv = (isTerminal(thenStep), elseStep.map(isTerminal)) match {
      case (true, Some(false)) => elseOptEnv
      case (false, Some(true)) => thenOptEnv
      case _                   => thenOptEnv ++ elseOptEnv
    }

    val flagVar = tag.getOrElse("USE_FLAG", s"${targetVar}_flag")
    rebaseCondition(
      cond,
      Map(
        targetVar -> BinaryCondition(
          ReferenceExpression(Variable(flagVar, None)),
          Eq,
          if (checkType == MayAbrupt) EnumLiteral("abrupt")
          else EnumLiteral("normal"),
        ),
      ),
    ) match {
      case Some(newCond) =>
        config(mergedEnv) :+ TaggedStep(
          IfStep(newCond, newThen, newElse, cfg),
          tag,
        )
      // TODO Can we ignore ElseStep? If not, how can we handle it?
      case None =>
        config(mergedEnv) :+ newThen
    }
  }

  private def handleTaggedGeneric(
    cond: Condition,
    thenStep: Step,
    elseStep: Option[Step],
    cfg: IfStep.ElseConfig,
    tag: Map[String, String],
    config: Config,
    rewriter: Rewriter,
  ): Config = {
    val env = config.env
    val Config(thenOptEnv, thenSteps) = rewriter.transform(thenStep, Config(env))
    val newThen = thenSteps.toBlockStep
    val (newElse, elseOptEnv) = elseStep match {
      case Some(e) =>
        val Config(eEnv, steps) = rewriter.transform(e, Config(env))
        (Some(steps.toBlockStep), eEnv)
      case None => (None, env)
    }
    val mergedEnv = (isTerminal(thenStep), elseStep.map(isTerminal)) match {
      case (true, Some(false) | None) => elseOptEnv
      case (false, Some(true))        => thenOptEnv
      case _                          => thenOptEnv ++ elseOptEnv
    }
    rebaseCondition(cond, Map()) match {
      case Some(newCond) =>
        config(mergedEnv) :+ TaggedStep(
          IfStep(newCond, newThen, newElse, cfg),
          tag,
        )
      // TODO Can we ignore ElseStep? If not, how can we handle it?
      case None =>
        config(mergedEnv) :+ newThen
    }
  }
}
