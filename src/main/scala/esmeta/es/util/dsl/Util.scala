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
  /** Maps variable name → reference it was copied from (valid only if no
    * mutation of the source occurred between copy and use).
    */
  copyOf: Map[String, Reference] = Map.empty,
)
type LangElemPredicate = (LangElem, DSLContext) => Boolean

// ---------------------------------------------------------------------------
// Rules — one variant per syntactic category for type safety
// ---------------------------------------------------------------------------
sealed trait Rule { def name: String }

/** Step-level rule. replace = None means delete. dynamicSubrules: generates
  * additional sub-rules from captured bindings (e.g., variable renaming
  * parameterized by matched values).
  */
case class StepRule(
  name: String,
  pattern: Step,
  replace: Option[Step],
  predicates: Map[String, LangElemPredicate] = Map.empty,
  subrules: List[Rule] = List.empty,
  dynamicSubrules: CaptureEnv => List[Rule] = _ => List.empty,
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

/** StepBlock-level rule for multi-step sequence matching. dynamicReplace: when
  * set, used instead of static replace+subrules. Receives captured bindings,
  * DSLContext, and stats for procedural transforms (e.g., early-return
  * wrapping).
  */
case class StepBlockRule(
  name: String,
  patternSteps: List[Step],
  replace: List[Step] = List.empty,
  predicates: Map[String, LangElemPredicate] = Map.empty,
  subrules: List[Rule] = List.empty,
  dynamicReplace: Option[
    (CaptureEnv, DSLContext, Option[TransformStats]) => List[Step],
  ] = None,
) extends Rule

/** Where-propagation rule. Scans a StepBlock for a context step matching
  * `wherePattern`, captures bindings, keeps the step, then applies dynamically
  * generated rules to subsequent siblings.
  */
case class WhereRule(
  name: String,
  wherePattern: Step,
  mainRules: CaptureEnv => List[Rule],
  predicates: Map[String, LangElemPredicate] = Map.empty,
) extends Rule
