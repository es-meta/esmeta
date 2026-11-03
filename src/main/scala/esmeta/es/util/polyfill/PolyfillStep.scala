package esmeta.es.util.polyfill

import esmeta.es.util.polyfill.completion.CompletionType
import esmeta.lang.*

/** the intermediate step language of the polyfill pipeline */
sealed trait PolyfillStep

/** an algorithm whose body has been lifted into the pipeline language */
case class PolyfillAlgo(
  name: String,
  head: esmeta.spec.Head,
  body: PolyfillStep,
)

object PolyfillStep {

  /** a metalanguage step carried as-is */
  case class LangStep(step: Step) extends PolyfillStep

  /** a sequence of steps, replacing [[esmeta.lang.BlockStep]] */
  case class Steps(steps: List[PolyfillStep]) extends PolyfillStep

  /** a conditional whose branches may contain polyfill-specific nodes */
  case class Branch(
    cond: Condition,
    thenStep: PolyfillStep,
    elseStep: Option[PolyfillStep],
    elseConfig: IfStep.ElseConfig,
  ) extends PolyfillStep

  /** a loop whose body may contain polyfill-specific nodes */
  case class Loop(header: Step, body: PolyfillStep) extends PolyfillStep

  case class Let(variable: Variable, expr: PolyfillExpr) extends PolyfillStep
  case class Assign(ref: Reference, expr: PolyfillExpr) extends PolyfillStep
  case class Perform(expr: PolyfillExpr) extends PolyfillStep
  case class Return(expr: PolyfillExpr) extends PolyfillStep

  /** a re-throw of a value already known to be an abrupt completion */
  case class Rethrow(name: String) extends PolyfillStep

  /** a completion check awaiting rebasing onto a completion flag
    *
    * Produced by `CompletionCheckRule` and consumed by
    * `CompletionCheckTransform`, which rewrites `cond` into a test of `flagVar`
    * and rewrites each branch under the completion type that branch implies.
    */
  case class CompletionCheck(
    targetVar: String,
    checkType: CompletionType,
    flagVar: String,
    cond: Condition,
    thenStep: PolyfillStep,
    elseStep: Option[PolyfillStep],
    elseConfig: IfStep.ElseConfig,
  ) extends PolyfillStep

  /** a try-catch introduced by completion erasure */
  case class Wrapped(
    tryBlock: PolyfillStep,
    catchVar: Reference,
    catchBlock: Option[PolyfillStep],
  ) extends PolyfillStep

  val blankBody: Step = BlockStep(StepBlock(Nil))

  /** lift a metalanguage step into the pipeline language */
  def lift(step: Step): PolyfillStep = step match {
    case LetStep(x, expr)   => Let(x, PolyfillExpr.lift(expr))
    case SetStep(ref, expr) => Assign(ref, PolyfillExpr.lift(expr))
    case PerformStep(expr)  => Perform(PolyfillExpr.lift(expr))
    case ReturnStep(expr)   => Return(PolyfillExpr.lift(expr))
    case IfStep(cond, thenStep, elseStep, elseConfig) =>
      Branch(cond, lift(thenStep), elseStep.map(lift), elseConfig)
    case BlockStep(StepBlock(subSteps)) =>
      Steps(subSteps.map(subStep => lift(subStep.step)))
    case s: RepeatStep         => Loop(s.copy(body = blankBody), lift(s.body))
    case s: ForEachStep        => Loop(s.copy(body = blankBody), lift(s.body))
    case s: ForEachIntegerStep => Loop(s.copy(body = blankBody), lift(s.body))
    case s: ForEachOwnPropertyKeyStep =>
      Loop(s.copy(body = blankBody), lift(s.body))
    case s: ForEachParseNodeStep =>
      Loop(s.copy(body = blankBody), lift(s.body))
    case other => LangStep(other)
  }

  /** pack steps into a single step, flattening nested sequences */
  extension (iter: Iterable[PolyfillStep])
    def toSteps: PolyfillStep = iter.toList match {
      case (s: Steps) :: Nil => s
      case list =>
        Steps(list.flatMap {
          case Steps(steps) => steps
          case x            => List(x)
        })
    }

  /** whether a step always leaves the enclosing algorithm */
  def isTerminal(step: PolyfillStep): Boolean = step match {
    case Return(_)                => true
    case Rethrow(_)               => true
    case LangStep(step)           => isTerminalLang(step)
    case Steps(steps)             => steps.lastOption.exists(isTerminal)
    case Branch(_, t, Some(e), _) => isTerminal(t) && isTerminal(e)
    case Wrapped(t, _, Some(c))   => isTerminal(t) && isTerminal(c)
    case CompletionCheck(_, _, _, _, t, Some(e), _) =>
      isTerminal(t) && isTerminal(e)
    case _ => false
  }

  /** whether a metalanguage step always leaves the enclosing algorithm */
  private def isTerminalLang(step: Step): Boolean = step match {
    case ReturnStep(_) => true
    case ThrowStep(_)  => true
    case BlockStep(StepBlock(steps)) =>
      steps.lastOption.exists(it => isTerminalLang(it.step))
    case IfStep(_, t, Some(e), _) => isTerminalLang(t) && isTerminalLang(e)
    case _                        => false
  }

}

/** the expression language paired with [[PolyfillStep]] */
sealed trait PolyfillExpr

object PolyfillExpr {

  /** a metalanguage expression carried as-is */
  case class LangExpr(expr: Expression) extends PolyfillExpr

  /** a closure whose body may contain polyfill-specific nodes */
  case class Closure(
    params: List[Variable],
    captured: List[Variable],
    body: PolyfillStep,
  ) extends PolyfillExpr

  /** an abstract-operation call, reified so a closure may be an argument */
  case class Invoke(
    name: String,
    args: List[PolyfillExpr],
    tag: HtmlTag,
  ) extends PolyfillExpr

  /** lift a metalanguage expression into the pipeline language */
  def lift(expr: Expression): PolyfillExpr = expr match {
    case AbstractClosureExpression(params, captured, body) =>
      Closure(params, captured, PolyfillStep.lift(body))
    case InvokeAbstractOperationExpression(name, args, tag) =>
      Invoke(name, args.map(lift), tag)
    case other => LangExpr(other)
  }
}
