package esmeta.lang

import esmeta.lang.util.*

// metalanguage steps
sealed trait Step extends Syntax
object Step extends Parser.From[Step]

// let steps
case class LetStep(variable: Variable, expr: Expression) extends Step

// set steps
case class SetStep(ref: Reference, expr: Expression) extends Step

// if-then-else steps
case class IfStep(cond: Condition, thenStep: Step, elseStep: Option[Step])
  extends Step

// return steps
case class ReturnStep(expr: Option[Expression]) extends Step

// assertion steps
case class AssertStep(cond: Condition) extends Step

// for-each steps
case class ForEachStep(
  ty: Option[Type],
  variable: Variable,
  expr: Expression,
  ascending: Boolean,
  body: Step,
) extends Step

// for-each steps for integers
case class ForEachIntegerStep(
  variable: Variable,
  start: Expression,
  cond: Condition,
  ascending: Boolean,
  body: Step,
) extends Step

// for-each steps for array index property
case class ForEachArrayIndexStep(
  key: Variable,
  array: Variable,
  start: Expression,
  ascending: Boolean,
  body: Step,
) extends Step

// throw steps
case class ThrowStep(errorName: String) extends Step

// perform steps
case class PerformStep(expr: Expression) extends Step

// append steps
case class AppendStep(elem: Expression, ref: Reference) extends Step

// repeat steps
case class RepeatStep(cond: Option[Condition], body: Step) extends Step

// push context steps
case class PushCtxtStep(ref: Reference) extends Step

// note steps
case class NoteStep(note: String) extends Step

// suspend steps
case class SuspendStep(context: Reference, remove: Boolean) extends Step

// set the code evaluation state steps
case class SetEvaluationStateStep(
  context: Reference,
  param: Option[Variable], // TODO handle type
  body: Step,
) extends Step

// resume the suspended evaluation steps
case class ResumeEvaluationStep(
  context: Reference,
  argument: Option[Expression],
  param: Option[Variable], // TODO handle type
  steps: List[SubStep],
) extends Step

// return to the resumed step
case class ReturnToResumeStep(
  context: Reference,
  returnStep: ReturnStep,
) extends Step

// block steps
case class BlockStep(block: StepBlock) extends Step

// not yet supported steps
case class YetStep(expr: YetExpression) extends Step
