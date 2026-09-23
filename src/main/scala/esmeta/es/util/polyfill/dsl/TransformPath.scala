package esmeta.es.util.polyfill.dsl

import esmeta.es.util.polyfill.completion.rules.inlining.ParameterInlineWalker
import esmeta.lang.*
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

}
