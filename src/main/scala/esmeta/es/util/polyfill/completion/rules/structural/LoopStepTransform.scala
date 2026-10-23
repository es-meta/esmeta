package esmeta.es.util.polyfill.completion.rules.structural

import esmeta.es.util.polyfill.*
import esmeta.es.util.polyfill.completion.*
import esmeta.es.util.polyfill.PolyfillStep.*
import esmeta.lang.*

/** rewrite the body of a loop step */
object LoopStepTransform extends EraseRule {
  def apply(
    step: PolyfillStep,
    config: Config,
    rewriter: Rewriter,
  ): Option[Config] =
    step match {
      case Loop(header, body) =>
        Some(config :+ Loop(header, rewriter.transformBlock(body, config)))
      case _ => None
    }
}
