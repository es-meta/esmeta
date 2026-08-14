package esmeta.es.util.polyfill

import esmeta.es.util.polyfill.CompletionType.*
import esmeta.es.util.polyfill.analysis.ValueAccessUnwrapper
import esmeta.lang.Step
import esmeta.util.BaseUtils.*

/** completion type of a variable */
enum CompletionType {
  case MayCompletion
  case MayNormal
  case MayAbrupt
  case NotCompletion

  def toTag: String = this match
    case MayNormal => "normal"
    case MayAbrupt => "abrupt"
    case other     => raise(s"Unexpected completion type in tag: $other")
  def join(that: CompletionType): CompletionType = (this, that) match {
    case _ if this == that  => this
    case (NotCompletion, _) => that
    case (_, NotCompletion) => this
    case _                  => MayCompletion
  }
}
object CompletionType {
  def fromTag(s: String): CompletionType = s match {
    case "normal" => MayNormal
    case "abrupt" => MayAbrupt
    case other    => raise(s"Unknown completion tag: $other")
  }
}

/** completion types of variables in scope */
case class TypeEnv(map: Map[String, CompletionType] = Map.empty) {
  def +(pair: (String, CompletionType)): TypeEnv = copy(map = map + pair)
  def -(name: String): TypeEnv = copy(map = map - name)
  def apply(name: String): CompletionType = map.getOrElse(name, NotCompletion)
  def ++(that: TypeEnv): TypeEnv = TypeEnv(
    (this.map.keySet ++ that.map.keySet).toList.map { key =>
      key -> (this(key) join that(key))
    }.toMap,
  )
}

/** state threaded through the rewriting: the current type environment and the
  * steps emitted so far
  */
case class Config(
  env: TypeEnv = TypeEnv(),
  steps: Vector[Step] = Vector.empty,
) {
  def clear: Config = copy(steps = Vector.empty)
  def apply(env: TypeEnv): Config = copy(env = env)
  def apply(name: String): CompletionType = env(name)
  def +(pair: (String, CompletionType)): Config = copy(env = env + pair)
  def :+(step: Step): Config = copy(steps = steps :+ unwrap(step))
  def ++(steps: Vector[Step]): Config = copy(steps = this.steps ++ steps)
  def unwrap(step: Step): Step = ValueAccessUnwrapper(env).walk(step)
}