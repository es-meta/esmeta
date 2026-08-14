package esmeta.es.util.polyfill.rules.inlining

import esmeta.es.util.polyfill.*
import esmeta.lang.*
import esmeta.lang.util.Walker as LangWalker

/** inline the body of an invoked shorthand, with its parameters substituted by
  * the given arguments
  */
object ShorthandInliningRule extends StepRule {
  def apply(step: Step, config: Config, rewriter: Rewriter): Option[Config] =
    step match {
      case InvokeShorthandStep(name, args) =>
        rewriter.algos.find(_.name == name) match {
          case None => Some(config :+ step)
          case Some(targetAlgo) =>
            val targetParameters = targetAlgo.head.originalParams.map(_.name)
            val inlinedStep =
              (targetParameters zip args).foldLeft(targetAlgo.body) {
                case (step, (param, arg)) =>
                  ParameterInlineWalker(param, arg).walk(step)
              }
            Some(rewriter.transform(inlinedStep, config.clear))
        }
      case _ => None
    }
}

/** replace references to a parameter with the given expression */
private class ParameterInlineWalker(
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