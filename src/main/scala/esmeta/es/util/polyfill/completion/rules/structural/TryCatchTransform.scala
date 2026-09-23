package esmeta.es.util.polyfill.completion.rules.structural

import esmeta.es.util.polyfill.*
import esmeta.es.util.polyfill.completion.*
import esmeta.es.util.polyfill.PolyfillStep.*
import esmeta.lang.*

/** rewrite the try and catch blocks of a try-catch step */
object TryCatchTransform extends EraseRule {
  def apply(
    step: PolyfillStep,
    config: Config,
    rewriter: Rewriter,
  ): Option[Config] =
    step match {
      case Wrapped(tryBlock, catchVar, catchBlock) =>
        val newTry = rewriter.transformBlock(tryBlock, config)
        val newCatch = catchBlock.map(rewriter.transformBlock(_, config))
        Some(config :+ Wrapped(newTry, catchVar, newCatch))
      case _ => None
    }
}
