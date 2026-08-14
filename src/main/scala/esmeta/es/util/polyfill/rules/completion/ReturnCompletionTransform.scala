package esmeta.es.util.polyfill.rules.completion

import esmeta.es.util.polyfill.*
import esmeta.es.util.polyfill.CompletionType.*
import esmeta.lang.*
import esmeta.lang.BinaryConditionOperator.Eq

/** guard a return of a possibly abrupt completion with a check on its flag */
object ReturnCompletionTransform extends StepRule {
  def apply(step: Step, config: Config, rewriter: Rewriter): Option[Config] =
    step match {
      // return ? x — ShorthandInliningRule only covers `? x` as a standalone
      // step; `return ? x` is ReturnStep(ReturnIfAbruptExpression(...)) and
      // needs explicit handling.
      case ReturnStep(
            ReturnIfAbruptExpression(
              ReferenceExpression(Variable(name, _, _, _)),
              true,
            ),
          ) =>
        Some(
          config :+ guarded(
            name,
            ReturnStep(ReferenceExpression(Variable(name, None))),
          ),
        )
      // return x, where x may be a completion
      case ret @ ReturnStep(ReferenceExpression(Variable(name, _, _, _)))
          if config(name) != NotCompletion =>
        Some(config :+ guarded(name, ret))
      case _ => None
    }

  /** if the flag of `name` is abrupt, throw it; otherwise, take `orElse` */
  private def guarded(name: String, orElse: Step): Step = IfStep(
    BinaryCondition(
      ReferenceExpression(Variable(s"${name}_flag", None)),
      Eq,
      EnumLiteral("abrupt"),
    ),
    TaggedStep(ThrowStep(name), Map("reason" -> "abrupt")),
    Some(orElse),
  )
}
