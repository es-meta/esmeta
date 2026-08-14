package esmeta.es.util.polyfill.rules.completion

import esmeta.es.util.polyfill.*
import esmeta.es.util.polyfill.CompletionType.*
import esmeta.es.util.polyfill.PolyfillInspector.*
import esmeta.es.util.polyfill.analysis.CompletionCheckPattern
import esmeta.lang.*

/** rewrite an explicit completion check (`if x is an abrupt completion, ...`)
  *
  * The check is dropped when the checked variable is already known to be normal
  * and nothing depends on the else branch; otherwise it is tagged so that
  * [[TaggedStepTransform]] can rebase it on the completion flag of the variable.
  */
object CompletionCheckRule extends StepRule {
  def apply(step: Step, config: Config, rewriter: Rewriter): Option[Config] =
    step match {
      case ifStep: IfStep =>
        CompletionCheckPattern.unapply(ifStep).map { (checkType, targetVar) =>
          val newConfig = config + (targetVar -> checkType)
          val canOmit =
            ifStep.elseStep.isEmpty && config(targetVar) == MayNormal
          if (canOmit) rewriter.transform(ifStep.thenStep, newConfig)
          else {
            val flagName = s"${targetVar}_flag"
            val taggedCheck = annotateStep(
              annotateStep(
                annotateStep(ifStep, "USE_FLAG", flagName),
                "TYPE",
                checkType.toTag,
              ),
              "TARGET_VAR",
              targetVar,
            )
            rewriter.transform(taggedCheck, newConfig)
          }
        }
      case _ => None
    }
}
