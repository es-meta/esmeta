package esmeta.es.util.polyfill.rules.completion

import esmeta.es.util.polyfill.*
import esmeta.es.util.polyfill.CompletionType.*
import esmeta.lang.*

/** turn a return of a known abrupt completion into an unconditional throw */
object ReturnThrowTransform extends StepRule {
  def apply(step: Step, config: Config, rewriter: Rewriter): Option[Config] =
    step match {
      // return ThrowCompletion(x)
      case ReturnStep(
            InvokeAbstractOperationExpression(
              "ThrowCompletion",
              ReferenceExpression(Variable(name, _, _, _)) :: Nil,
              _,
            ),
          ) =>
        Some(config :+ throwStep(name))
      // return x, where x is known to be abrupt
      case ReturnStep(ReferenceExpression(Variable(name, _, _, _)))
          if config(name) == MayAbrupt =>
        Some(config :+ throwStep(name))
      case _ => None
    }

  private def throwStep(name: String): Step =
    TaggedStep(ThrowStep(name), Map("reason" -> "abrupt"))
}
