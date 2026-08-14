package esmeta.es.util.polyfill.rules.inlining

import esmeta.es.util.polyfill.*
import esmeta.lang.*
import esmeta.spec.*

/** turn a `let x be <xref>` step referring to an algorithm into a closure
  * holding the rewritten body of that algorithm
  */
object XRefInliningRule extends StepRule {
  def apply(step: Step, config: Config, rewriter: Rewriter): Option[Config] =
    step match {
      case LetStep(
            Variable(x, _, _, _),
            XRefExpression(XRefExpressionOperator.Algo, id),
          ) =>
        val targetFunction = rewriter.algos.find(_.name.endsWith(id))
        Some(targetFunction.fold(config :+ step) { func =>
          val extractedHead = func.head.asInstanceOf[BuiltinHead]
          val optimizedClosureBody = rewriter.transformBlock(func.body, Config())
          val params = extractedHead.params
          val closureExpression = AbstractClosureExpression(
            params.map(it => Variable(it.name, Some("xref_inlined"))),
            Nil,
            optimizedClosureBody,
          )
          config :+ LetStep(Variable(x), closureExpression)
        })
      case _ => None
    }
}
