package esmeta.es.util.polyfill.rules.structural

import esmeta.es.util.polyfill.*
import esmeta.lang.*

/** rewrite the try and catch blocks of a try-catch step */
object TryCatchTransform extends StepRule {
  def apply(step: Step, config: Config, rewriter: Rewriter): Option[Config] =
    step match {
      case WrappedTryCatchStep(tryBlock, catchVar, catchBlock) =>
        val newTry = rewriter.transformBlock(tryBlock, config)
        val newCatch = catchBlock.map(rewriter.transformBlock(_, config))
        Some(config :+ WrappedTryCatchStep(newTry, catchVar, newCatch))
      case _ => None
    }
}
