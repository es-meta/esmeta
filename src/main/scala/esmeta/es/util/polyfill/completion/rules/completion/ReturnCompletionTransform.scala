package esmeta.es.util.polyfill.completion.rules.completion

import esmeta.es.util.polyfill.*
import esmeta.es.util.polyfill.completion.*
import esmeta.es.util.polyfill.completion.CompletionType.*
import esmeta.es.util.polyfill.PolyfillExpr.*
import esmeta.es.util.polyfill.PolyfillStep.*
import esmeta.lang.*
import esmeta.lang.BinaryConditionOperator.Eq

/** guard a return of a possibly abrupt completion with a check on its flag */
object ReturnCompletionTransform extends EraseRule {
  def apply(
    step: PolyfillStep,
    config: Config,
    rewriter: Rewriter,
  ): Option[Config] =
    step match {
      // return ? x — ShorthandInliningRule only covers `? x` as a standalone
      // step; `return ? x` is Return(ReturnIfAbruptExpression(...)) and needs
      // explicit handling.
      case Return(
            LangExpr(
              ReturnIfAbruptExpression(
                ReferenceExpression(Variable(name, _, _, _)),
                true,
              ),
            ),
          ) =>
        Some(
          config :+ guarded(
            name,
            Return(
              LangExpr(ReferenceExpression(Variable(name, None))),
            ),
          ),
        )
      // return x, where x may be a completion
      case ret @ Return(
            LangExpr(ReferenceExpression(Variable(name, _, _, _))),
          ) if config(name) != NotCompletion =>
        Some(config :+ guarded(name, ret))
      case _ => None
    }

  /** if the flag of `name` is abrupt, throw it; otherwise, take `orElse` */
  private def guarded(name: String, orElse: PolyfillStep): PolyfillStep =
    Branch(
      BinaryCondition(
        ReferenceExpression(Variable(s"${name}_flag", None)),
        Eq,
        EnumLiteral("abrupt"),
      ),
      Rethrow(name),
      Some(orElse),
      IfStep.ElseConfig(),
    )
}
