package esmeta.es.util.polyfill.completion.rules.completion

import esmeta.es.util.polyfill.*
import esmeta.es.util.polyfill.completion.*
import esmeta.es.util.polyfill.completion.CompletionType.*
import esmeta.es.util.polyfill.PolyfillStep.*
import esmeta.lang.*

/** rewrite an explicit completion check (`if x is an abrupt completion, ...`)
  *
  * The check is dropped when the checked variable is already known to be normal
  * and nothing depends on the else branch; otherwise it is tagged so that
  * [[CompletionCheckTransform]] can rebase it on the completion flag of the
  * variable.
  */
object CompletionCheckRule extends EraseRule {
  def apply(
    step: PolyfillStep,
    config: Config,
    rewriter: Rewriter,
  ): Option[Config] =
    step match {
      case Branch(cond, thenStep, elseStep, elseConfig) =>
        cond match {
          case cond @ CompletionCheckPattern(checkType, targetVar) =>
            val newConfig = config + (targetVar -> checkType)
            val canOmit = elseStep.isEmpty && config(targetVar) == MayNormal
            if (canOmit) Some(rewriter.transform(thenStep, newConfig))
            else
              Some(
                rewriter.transform(
                  CompletionCheck(
                    targetVar,
                    checkType,
                    s"${targetVar}_flag",
                    cond,
                    thenStep,
                    elseStep,
                    elseConfig,
                  ),
                  newConfig,
                ),
              )
          case _ => None
        }

      case _ => None
    }
}
