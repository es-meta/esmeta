package esmeta.es.util.polyfill.rules.structural

import esmeta.es.util.polyfill.*
import esmeta.es.util.polyfill.PolyfillInspector.*
import esmeta.lang.*

/** rewrite both branches of an `if` step and join their environments */
object IfStepTransform extends StepRule {
  def apply(step: Step, config: Config, rewriter: Rewriter): Option[Config] =
    step match {
      case IfStep(cond, t, e, cfg) =>
        val Config(thenEnv, thenSteps) = rewriter.transform(t, config.clear)
        val (elseResult, elseEnv) = e match {
          case Some(b) =>
            val Config(eEnv, steps) = rewriter.transform(b, config.clear)
            if (steps.isEmpty) (None, eEnv)
            else (Some(steps.toBlockStep), eEnv)
          case None => (None, config.env)
        }
        val mergedEnv = thenEnv ++ elseEnv
        Some(
          config(mergedEnv) :+ IfStep(
            cond,
            thenSteps.toBlockStep,
            elseResult,
            cfg,
          ),
        )
      case _ => None
    }
}
