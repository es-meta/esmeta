package esmeta.es.util.dsl

import esmeta.lang.*

object MetaVar {
  def unapply(v: Variable): Option[String] =
    if (v.name.startsWith("$")) Some(v.name) else None
}

object AstExtensions {
  extension (block: StepBlock) {
    def rawSteps: List[Step] = block.steps.map(_.step)
  }

  extension (steps: List[Step]) {
    def subSteps: List[SubStep] = steps.map(SubStep(None, _))
    def stepBlock: StepBlock = StepBlock(steps.subSteps)
    def blockStep: BlockStep = BlockStep(steps.stepBlock)
  }
}

type CaptureEnv = Map[String, LangElem]

case class DSLContext(
  variableTypes: Map[String, String],
  /** Maps variable name -> reference it was copied from (valid only if no
    * mutation of the source occurred between copy and use).
    */
  copyOf: Map[String, Reference] = Map.empty,
)
type LangElemPredicate = (LangElem, DSLContext) => Boolean

// ---------------------------------------------------------------------------
// Rules — one variant per syntactic category for type safety
// All rules are fully declarative (no lambdas).
// ---------------------------------------------------------------------------
sealed trait Rule { def name: String }

/** Step-level rule. replace = None means delete. */
case class StepRule(
  name: String,
  pattern: Step,
  replace: Option[Step] = None,
  predicates: Map[String, LangElemPredicate] = Map.empty,
  subrules: List[Rule] = List.empty,
) extends Rule

/** Expression-level rule. */
case class ExpressionRule(
  name: String,
  pattern: Expression,
  replace: Expression,
  predicates: Map[String, LangElemPredicate] = Map.empty,
) extends Rule

/** Condition-level rule. */
case class ConditionRule(
  name: String,
  pattern: Condition,
  replace: Condition,
  predicates: Map[String, LangElemPredicate] = Map.empty,
) extends Rule

/** Reference-level rule. */
case class ReferenceRule(
  name: String,
  pattern: Reference,
  replace: Reference,
  predicates: Map[String, LangElemPredicate] = Map.empty,
) extends Rule

/** Configuration for closure wrapping with optional early-return. */
case class ClosureConfig(
  aoName: String,
  iterBase: String,
  elementVar: String,
  bodyHole: String = "$body",
  earlyReturn: Boolean = false,
)

/** StepBlock-level rule for multi-step sequence matching. When closureConfig is
  * set, the engine wraps the matched body in a closure call (applying subrules
  * first, then EarlyReturn if configured).
  */
case class StepBlockRule(
  name: String,
  patternSteps: List[Step],
  replace: List[Step] = List.empty,
  predicates: Map[String, LangElemPredicate] = Map.empty,
  subrules: List[Rule] = List.empty,
  closureConfig: Option[ClosureConfig] = None,
  copyCheck: Option[(String, String)] = None,
) extends Rule

/** Where-propagation rule. Scans a StepBlock for a context step matching
  * `wherePattern`, captures bindings, keeps the step, then applies mainRules
  * (pre-substituted with where bindings) to subsequent siblings.
  */
case class WhereRule(
  name: String,
  wherePattern: Step,
  mainRules: List[Rule],
  predicates: Map[String, LangElemPredicate] = Map.empty,
) extends Rule
