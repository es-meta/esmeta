package esmeta.es.util.polyfill.completion.rules.structural

import esmeta.es.util.polyfill.*
import esmeta.es.util.polyfill.completion.*
import esmeta.es.util.polyfill.PolyfillStep.*

/** rewrite the sub-steps of a block, threading the environment through them */
object BlockStepTransform extends EraseRule {
  def apply(
    step: PolyfillStep,
    config: Config,
    rewriter: Rewriter,
  ): Option[Config] =
    step match {
      case Steps(stmts) =>
        val Config(newEnv, newSteps) = stmts.foldLeft(config.clear) {
          case (config, stmt) => rewriter.transform(stmt, config)
        }
        Some(
          if (newSteps.isEmpty) config(newEnv)
          else config(newEnv) :+ newSteps.toSteps,
        )
      case _ => None
    }
}
