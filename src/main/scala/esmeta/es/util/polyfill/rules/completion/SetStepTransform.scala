package esmeta.es.util.polyfill.rules.completion

import esmeta.es.util.polyfill.*
import esmeta.es.util.polyfill.PolyfillInspector.*
import esmeta.lang.*

/** assign to a variable, wrapping the producer when it may yield a completion
  */
object SetStepTransform extends StepRule {
  def apply(step: Step, config: Config, rewriter: Rewriter): Option[Config] =
    step match {
      case SetStep(Variable(x, _, _, _), expr) =>
        val (newExpr, typeUpdate) = rewriter.transformExpr(expr, config)
        Some(
          wrap(
            config,
            x,
            newExpr,
            typeUpdate,
            isDecl = false,
          ) + (x -> typeUpdate),
        )
      case _ => None
    }
}
