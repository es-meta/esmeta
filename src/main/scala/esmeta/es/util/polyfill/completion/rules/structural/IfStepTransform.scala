package esmeta.es.util.polyfill.completion.rules.structural

import esmeta.es.util.polyfill.*
import esmeta.es.util.polyfill.completion.*
import esmeta.es.util.polyfill.PolyfillStep.*
import esmeta.lang.*

/** rewrite both branches of an `if` step and join their environments */
object IfStepTransform extends EraseRule {
  def apply(
    step: PolyfillStep,
    config: Config,
    rewriter: Rewriter,
  ): Option[Config] =
    step match {
      case Branch(cond, t, e, cfg) =>
        val Config(thenEnv, thenSteps) = rewriter.transform(t, config.clear)
        val (elseResult, elseEnv) = e match {
          case Some(b) =>
            val Config(eEnv, steps) = rewriter.transform(b, config.clear)
            if (steps.isEmpty) (None, eEnv)
            else (Some(steps.toSteps), eEnv)
          case None => (None, config.env)
        }
        val mergedEnv = thenEnv ++ elseEnv
        Some(
          config(mergedEnv) :+ Branch(
            cond,
            thenSteps.toSteps,
            elseResult,
            cfg,
          ),
        )
      case _ => None
    }
}
