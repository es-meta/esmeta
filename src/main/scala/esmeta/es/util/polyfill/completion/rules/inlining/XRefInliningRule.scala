package esmeta.es.util.polyfill.completion.rules.inlining

import esmeta.es.util.polyfill.*
import esmeta.es.util.polyfill.completion.*
import esmeta.es.util.polyfill.PolyfillStep.*
import esmeta.lang.*

/** turn a `let x be <xref>` step referring to an algorithm into a closure
  * holding the rewritten body of that algorithm
  */
object XRefInliningRule extends EraseRule {
  def apply(
    step: PolyfillStep,
    config: Config,
    rewriter: Rewriter,
  ): Option[Config] =
    step match {
      case Let(
            Variable(x, _, _, _),
            PolyfillExpr.LangExpr(
              XRefExpression(XRefExpressionOperator.Algo, id),
            ),
          ) =>
        val targetFunction = rewriter.algos.find(_.name.endsWith(id))
        Some(targetFunction.fold(config :+ step) { func =>
          val optimizedClosureBody =
            rewriter.transformBlock(lift(func.body), Config())
          val params = func.head.originalParams
          val closure = PolyfillExpr.Closure(
            params.map(it => Variable(it.name, Some("xref_inlined"))),
            Nil,
            optimizedClosureBody,
          )
          config :+ Let(Variable(x), closure)
        })
      case _ => None
    }
}
