package esmeta.es.util.polyfill.rules.structural

import esmeta.es.util.polyfill.*
import esmeta.lang.*

/** rewrite the body of a loop step
  *
  * The body is rewritten in a fresh step buffer, and the environment it produces
  * is dropped since the loop may run any number of times.
  */
object LoopStepTransform extends StepRule {
  def apply(step: Step, config: Config, rewriter: Rewriter): Option[Config] =
    step match {
      case RepeatStep(c, b) =>
        Some(config :+ RepeatStep(c, rewriter.transformBlock(b, config)))
      case s @ ForEachStep(_, _, _, _, body) =>
        Some(config :+ s.copy(body = rewriter.transformBlock(body, config)))
      case s @ ForEachIntegerStep(_, _, _, _, _, _, body) =>
        Some(config :+ s.copy(body = rewriter.transformBlock(body, config)))
      case s @ ForEachOwnPropertyKeyStep(_, _, _, _, _, body) =>
        Some(config :+ s.copy(body = rewriter.transformBlock(body, config)))
      case s @ ForEachParseNodeStep(_, _, body) =>
        Some(config :+ s.copy(body = rewriter.transformBlock(body, config)))
      case _ => None
    }
}
