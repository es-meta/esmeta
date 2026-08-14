package esmeta.es.util.polyfill.rules.structural

import esmeta.es.util.polyfill.*
import esmeta.es.util.polyfill.PolyfillInspector.*
import esmeta.lang.*

/** rewrite the sub-steps of a block, threading the environment through them */
object BlockStepTransform extends StepRule {
  def apply(step: Step, config: Config, rewriter: Rewriter): Option[Config] =
    step match {
      case BlockStep(StepBlock(stmts)) =>
        val Config(newEnv, newSteps) = stmts.foldLeft(config.clear) {
          case (config, stmt) => rewriter.transform(stmt.step, config)
        }
        Some(
          if (newSteps.isEmpty) config(newEnv)
          else config(newEnv) :+ newSteps.toBlockStep,
        )
      case _ => None
    }
}
