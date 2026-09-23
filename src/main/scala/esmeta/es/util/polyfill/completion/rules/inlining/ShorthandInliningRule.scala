package esmeta.es.util.polyfill.completion.rules.inlining

import esmeta.es.util.polyfill.*
import esmeta.es.util.polyfill.completion.*
import esmeta.es.util.polyfill.PolyfillStep.*
import esmeta.lang.*
import esmeta.lang.util.Walker as LangWalker

/** inline the body of an invoked shorthand, with its parameters substituted by
  * the given arguments
  */
object ShorthandInliningRule extends EraseRule {
  def apply(
    step: PolyfillStep,
    config: Config,
    rewriter: Rewriter,
  ): Option[Config] =
    step match {
      case LangStep(InvokeShorthandStep(name, args)) =>
        rewriter.algos.find(_.name == name) match {
          case None => Some(config :+ step)
          case Some(targetAlgo) =>
            val targetParameters = targetAlgo.head.originalParams.map(_.name)
            val inlinedStep =
              (targetParameters zip args).foldLeft(targetAlgo.body) {
                case (step, (param, arg)) =>
                  ParameterInlineWalker(param, arg).walk(step)
              }
            Some(rewriter.transform(lift(inlinedStep), config.clear))
        }
      case _ => None
    }
}

/** replace references to a parameter with the given expression
  *
  * Shared with [[esmeta.es.util.polyfill.ShorthandInlinePath]], which inlines
  * the same shorthands one pass earlier.
  */
class ParameterInlineWalker(
  paramName: String,
  replaceWith: Expression,
) extends LangWalker {
  override def walk(expr: Expression): Expression = expr match {
    case ReferenceExpression(ref) =>
      ref match {
        case Variable(name, None, _, _) =>
          if (name == paramName) replaceWith else expr
        case x => ReferenceExpression(walk(x))
      }
    case _ => super.walk(expr)
  }

  override def walk(ref: Reference): Reference = ref match {
    case Variable(name, _, _, _) =>
      if (name == paramName) {
        replaceWith.asInstanceOf[ReferenceExpression].ref
      } else ref
    case x => super.walk(x)
  }
}
