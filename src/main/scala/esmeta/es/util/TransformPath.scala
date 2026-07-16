package esmeta.es.util

import esmeta.lang.*
import esmeta.lang.util.UnitWalker as LangUnitWalker
import esmeta.lang.util.Walker as LangWalker
import esmeta.spec.*

trait TransformPath {
  def apply(targets: List[Algorithm]): List[Algorithm]
}

class ShorthandInlinePath(spec: Spec) extends TransformPath {
  override def apply(targets: List[Algorithm]): List[Algorithm] = {
    targets.map { algo =>
      val inlinedBody = new LangWalker {
        override def walk(step: Step): Step = step match
          case InvokeShorthandStep(name, args) =>
            val shorthandAlgo = spec.fnameMap(name)
            val targetParameters = shorthandAlgo.head.originalParams.map(_.name)
            (targetParameters zip args).foldLeft(shorthandAlgo.body) {
              case (acc, (param, arg)) =>
                ParameterInlineWalker(param, arg).walk(acc)
            }
          case _ => super.walk(step)
      }.walk(algo.body)
      algo.copy(body = inlinedBody)
    }
  }

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
}

class CompletionPath extends TransformPath {
  override def apply(targets: List[Algorithm]): List[Algorithm] = {
    targets.map { algo =>
      val inspector = new PolyfillInspector(algo, targets)
      val newHead = inspector.transformHead
      val transformedBody = inspector.transformBody
      algo.copy(head = newHead, body = transformedBody)
    }
  }
}
