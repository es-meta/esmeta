package esmeta.es.util.polyfill

import esmeta.es.util.polyfill.CompletionType.*
import esmeta.lang.*
import esmeta.spec.Algorithm

/** a rewriting rule for a single step
  *
  * A rule returns `None` when it does not apply, so that the next rule of the
  * dispatcher is tried instead.
  */
trait StepRule {
  def apply(step: Step, config: Config, rewriter: Rewriter): Option[Config]
}

/** rule dispatcher
  *
  * Rules are tried in order and the first one that applies wins, so a rule with
  * a more specific pattern must be placed before the more general one (e.g.
  * `XRefInliningRule` before `LetStepTransform`). Steps that no rule handles are
  * emitted as-is.
  */
class Rewriter(
  val rules: List[StepRule],
  val algos: List[Algorithm],
) {
  import PolyfillInspector.*

  /** rewrite a step and append the result to the given config */
  def transform(step: Step, config: Config): Config =
    rules.iterator
      .flatMap(_(step, config, this))
      .nextOption()
      .getOrElse(config :+ step)

  /** rewrite a nested step in a fresh step buffer, and pack it into a block */
  def transformBlock(step: Step, config: Config): Step =
    transform(step, config.clear).steps.toBlockStep

  /** rewrite an expression, along with the completion type it produces */
  def transformExpr(
    expr: Expression,
    config: Config,
  ): (Expression, CompletionType) = expr match {
    case InvokeAbstractOperationExpression("Completion", args, _) =>
      (args.head, MayCompletion)
    case InvokeAbstractOperationExpression("NormalCompletion", args, _) =>
      (args.head, MayNormal)
    case InvokeAbstractOperationExpression("ThrowCompletion", args, _) =>
      (args.head, MayAbrupt)
    case InvokeAbstractOperationExpression("AbruptCompletion", args, _) =>
      (args.head, MayAbrupt)
    case AbstractClosureExpression(params, captured, body) =>
      (
        AbstractClosureExpression(
          params,
          captured,
          transformBlock(body, config),
        ),
        NotCompletion,
      )
    case ReferenceExpression(Variable(name, _, _, _)) => (expr, config(name))
    case ReturnIfAbruptExpression(expr, _)            => (expr, NotCompletion)
    case _                                            => (expr, NotCompletion)
  }
}