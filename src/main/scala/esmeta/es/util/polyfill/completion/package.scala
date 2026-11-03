package esmeta.es.util.polyfill.completion

import esmeta.es.util.polyfill.PolyfillStep
import esmeta.es.util.polyfill.completion.CompletionType.*
import esmeta.lang.Step

/** completion type of a variable */
enum CompletionType {
  case MayCompletion
  case MayNormal
  case MayAbrupt
  case NotCompletion

  def join(that: CompletionType): CompletionType = (this, that) match {
    case _ if this == that  => this
    case (NotCompletion, _) => that
    case (_, NotCompletion) => this
    case _                  => MayCompletion
  }
}

/** completion types of variables in scope */
case class TypeEnv(map: Map[String, CompletionType] = Map.empty) {
  def +(pair: (String, CompletionType)): TypeEnv = copy(map = map + pair)
  def apply(name: String): CompletionType = map.getOrElse(name, NotCompletion)
  def ++(that: TypeEnv): TypeEnv = TypeEnv(
    (this.map.keySet ++ that.map.keySet).toList.map { key =>
      key -> (this(key) `join` that(key))
    }.toMap,
  )
}

case class Config(
  env: TypeEnv = TypeEnv(),
  steps: Vector[PolyfillStep] = Vector.empty,
) {
  def clear: Config = copy(steps = Vector.empty)
  def apply(env: TypeEnv): Config = copy(env = env)
  def apply(name: String): CompletionType = env(name)
  def +(pair: (String, CompletionType)): Config = copy(env = env + pair)
  def :+(step: Step): Config = this :+ PolyfillStep.LangStep(step)
  def :+(step: PolyfillStep): Config = copy(steps = steps :+ unwrap(step))
  def unwrap(step: PolyfillStep): PolyfillStep =
    ValueAccessUnwrapper(env).walk(step)
}

trait EraseRule {
  def apply(
    step: PolyfillStep,
    config: Config,
    rewriter: Rewriter,
  ): Option[Config]
}
