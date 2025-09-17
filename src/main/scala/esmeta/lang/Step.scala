package esmeta.lang

import esmeta.lang.util.*

// metalanguage steps
sealed trait Step extends Syntax {
  var endingChar = "." // "." or ";"
  lazy val isNextUpper = endingChar == ".";

  /** check whether it is complete */
  def complete: Boolean = this match
    case _: YetStep            => false
    case IfStep(cond, _, _, _) => cond.complete
    case AssertStep(cond)      => cond.complete
    case _                     => true
}
object Step extends Parser.From(Parser.step)

// let steps
case class LetStep(variable: Variable, expr: Expression) extends Step

// set steps
case class SetStep(ref: Reference, expr: Expression) extends Step

// set-as steps
case class SetAsStep(ref: Reference, verb: String, id: String) extends Step

// set-eval-state steps
case class SetEvaluationStateStep(
  context: Reference,
  func: Variable,
  args: List[Expression],
) extends Step

// perform steps
case class PerformStep(expr: Expression) extends Step

// invoke shorthand steps
case class InvokeShorthandStep(
  name: String,
  args: List[Expression],
) extends Step

// append steps
case class AppendStep(elem: Expression, ref: Reference) extends Step

// prepend steps
case class PrependStep(elem: Expression, ref: Reference) extends Step

// add steps
case class AddStep(elem: Expression, ref: Reference) extends Step

// remove element steps
case class RemoveStep(
  target: RemoveStep.Target,
  prep: String, // TODO: use only "from" and remove this field
  list: Expression,
) extends Step
object RemoveStep:
  enum Target:
    case First(count: Option[Expression])
    case Last(count: Option[Expression])
    case Element(elem: Expression)

// push context steps
case class PushContextStep(ref: Reference) extends Step

// suspend steps
case class SuspendStep(variable: Option[Variable], remove: Boolean) extends Step

// remove execution context steps
case class RemoveContextStep(
  context: Reference,
  restoreTarget: RemoveContextStep.RestoreTarget,
) extends Step
object RemoveContextStep:
  enum RestoreTarget:
    case NoRestore
    case StackTop
    case Context(ref: Reference)

// assertion steps
case class AssertStep(cond: Condition) extends Step

// if-then-else steps
case class IfStep(
  cond: Condition,
  thenStep: Step,
  elseStep: Option[Step],
  elseConfig: IfStep.ElseConfig = IfStep.ElseConfig(),
) extends Step
object IfStep:
  // TODO: simplify/remove config (https://github.com/tc39/ecma262/issues/3648)
  case class ElseConfig(
    newLine: Boolean = true,
    keyword: String = "else", // "else" or "otherwise"
    comma: Boolean = true,
  )

// repeat steps
case class RepeatStep(cond: RepeatStep.LoopCondition, body: Step) extends Step
object RepeatStep:
  enum LoopCondition:
    case NoCondition
    case While(cond: Condition)
    case Until(cond: Condition)

// for-each steps
case class ForEachStep(
  ty: Option[Type],
  variable: Variable,
  expr: Expression,
  forward: Boolean,
  body: Step,
) extends Step

// for-each steps for integers
case class ForEachIntegerStep(
  variable: Variable,
  low: Expression,
  lowInclusive: Boolean,
  high: Expression,
  highInclusive: Boolean,
  ascending: Boolean,
  body: Step,
) extends Step

// for-each steps for OwnPropertyKey
case class ForEachOwnPropertyKeyStep(
  key: Variable,
  obj: Variable,
  cond: Condition,
  ascending: Boolean,
  order: ForEachOwnPropertyKeyStepOrder,
  body: Step,
) extends Step
enum ForEachOwnPropertyKeyStepOrder extends LangElem:
  case NumericIndexOrder, ChronologicalOrder

// for-each steps for parse node
case class ForEachParseNodeStep(
  variable: Variable,
  expr: Expression,
  body: Step,
) extends Step

// return steps
case class ReturnStep(expr: Expression) extends Step

// throw steps
case class ThrowStep(name: String) extends Step

// resume steps for yield
case class ResumeStep(
  callerContext: Reference,
  argument: Expression,
  generatorContext: Reference,
  param: Variable,
  steps: List[SubStep],
) extends Step

// resume the suspended evaluation steps
case class ResumeEvaluationStep(
  context: Reference,
  argument: Option[Expression],
  param: Option[(Variable, String)],
  steps: List[SubStep],
) extends Step

// resume the top context
case class ResumeTopContextStep() extends Step

// note steps
case class NoteStep(note: String) extends Step

// block steps
case class BlockStep(block: StepBlock) extends Step

// not yet supported steps
case class YetStep(expr: YetExpression) extends Step

// -----------------------------------------------------------------------------
// special steps rarely used in the spec
// -----------------------------------------------------------------------------
// set fields with intrinsics
case class SetFieldsWithIntrinsicsStep(ref: Reference, desc: String)
  extends Step

// perform block steps
case class PerformBlockStep(step: StepBlock, desc: String) extends Step
