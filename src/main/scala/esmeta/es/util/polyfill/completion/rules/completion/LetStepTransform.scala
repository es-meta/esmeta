package esmeta.es.util.polyfill.completion.rules.completion

import esmeta.es.util.polyfill.*
import esmeta.es.util.polyfill.completion.*
import esmeta.es.util.polyfill.completion.Eraser.*
import esmeta.es.util.polyfill.PolyfillStep.*
import esmeta.lang.*

/** declare a variable, wrapping the producer when it may yield a completion */
object LetStepTransform extends EraseRule {
  def apply(
    step: PolyfillStep,
    config: Config,
    rewriter: Rewriter,
  ): Option[Config] =
    step match {
      case Let(Variable(x, _, _, _), expr) =>
        val (newExpr, typeUpdate) = rewriter.transformExpr(expr, config)
        Some(
          wrap(
            config,
            x,
            newExpr,
            typeUpdate,
            isDecl = true,
          ) + (x -> typeUpdate),
        )
      case _ => None
    }
}
