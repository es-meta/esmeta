package esmeta.es.util.polyfill.completion.rules.completion

import esmeta.es.util.polyfill.*
import esmeta.es.util.polyfill.completion.*
import esmeta.es.util.polyfill.completion.CompletionType.*
import esmeta.es.util.polyfill.PolyfillExpr.*
import esmeta.es.util.polyfill.PolyfillStep.*
import esmeta.lang.*

/** turn a return of a known abrupt completion into an unconditional throw */
object ReturnThrowTransform extends EraseRule {
  def apply(
    step: PolyfillStep,
    config: Config,
    rewriter: Rewriter,
  ): Option[Config] =
    step match {
      // return ThrowCompletion(x)
      case Return(
            Invoke(
              "ThrowCompletion",
              LangExpr(ReferenceExpression(Variable(name, _, _, _))) :: Nil,
              _,
            ),
          ) =>
        Some(config :+ Rethrow(name))
      // return x, where x is known to be abrupt
      case Return(LangExpr(ReferenceExpression(Variable(name, _, _, _))))
          if config(name) == MayAbrupt =>
        Some(config :+ Rethrow(name))
      case _ => None
    }
}
