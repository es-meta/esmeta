package esmeta.es.util.polyfill

import esmeta.es.util.polyfill.CompletionType.*
import esmeta.es.util.polyfill.rules.completion.*
import esmeta.es.util.polyfill.rules.inlining.*
import esmeta.es.util.polyfill.rules.structural.*
import esmeta.lang.*
import esmeta.lang.PredicateConditionOperator.*
import esmeta.spec.*
import esmeta.ty.ValueTy

// =============================================================================
// Entry Point
// =============================================================================

/** completion record erasure for a single algorithm
  *
  * Completion records are replaced by a pair of a plain value and a `x_flag`
  * variable holding its completion type, so that abrupt completions can be
  * represented by exceptions of the target language.
  */
class PolyfillInspector(algo: Algorithm, algos: List[Algorithm]) {
  val head = algo.head
  val body = algo.body

  /** rules of the rewriting, tried in order: a rule with a more specific
    * pattern must come before the more general one
    */
  private val rewriter = new Rewriter(
    rules = List(
      ShorthandInliningRule,
      XRefInliningRule,
      LetStepTransform,
      SetStepTransform,
      CompletionCheckRule,
      ReturnThrowTransform,
      ReturnCompletionTransform,
      IfStepTransform,
      TaggedStepTransform,
      BlockStepTransform,
      LoopStepTransform,
      TryCatchTransform,
    ),
    algos = algos,
  )

  /** split each completion parameter into its flag and its value */
  def transformHead: Head = head match {
    case ao @ AbstractOperationHead(_, _, params, _) =>
      val unwrapParams = params.flatMap {
        case p @ Param(name, Type(ty), paramKind) if ty.isCompletion =>
          List(
            p.copy(name = s"${name}_flag", ty = Type(ValueTy.Top)),
            p.copy(ty = Type(ValueTy.Top)),
          )
        case x => Some(x)
      }
      ao.copy(params = unwrapParams)
    case x => x
  }

  /** rewrite the body, starting from the completion types of the parameters */
  def transformBody: Step = {
    val env = TypeEnv((for {
      param <- head.originalParams
      if param.ty.ty.isCompletion
    } yield param.name -> MayCompletion).toMap)
    rewriter.transformBlock(body, Config(env))
  }
}

// =============================================================================
// Utilities shared across rules
// =============================================================================

object PolyfillInspector {

  extension (iter: Iterable[Step]) {
    def toBlockStep: Step = iter.toList match {
      case (b: BlockStep) :: Nil => b
      case list =>
        BlockStep(StepBlock(list.flatMap {
          case BlockStep(StepBlock(steps)) => steps
          case x                           => List(SubStep(None, x))
        }))
    }
  }

  /** whether a step always leaves the enclosing algorithm */
  def isTerminal(stmt: Step): Boolean = stmt match {
    case ReturnStep(_) => true
    case ThrowStep(_)  => true
    case BlockStep(StepBlock(steps)) =>
      steps.lastOption.exists(it => isTerminal(it.step))
    case IfStep(_, t, Some(e), _)           => isTerminal(t) && isTerminal(e)
    case WrappedTryCatchStep(t, _, Some(c)) => isTerminal(t) && isTerminal(c)
    case _                                  => false
  }

  def annotateStep(
    step: Step,
    name: String,
    value: String,
  ): TaggedStep = step match {
    case TaggedStep(s, tag) => TaggedStep(s, tag + (name -> value))
    case x                  => TaggedStep(x, Map(name -> value))
  }

  /** replace completion checks in a condition with checks on completion flags,
    * dropping the condition when it becomes vacuous
    */
  def rebaseCondition(
    cond: Condition,
    completionCondition: Map[String, Condition],
  ): Option[Condition] = cond match {
    case PredicateCondition(
          ReferenceExpression(Variable(targetVar, _, _, _)),
          _,
          op,
        ) =>
      op match {
        case Abrupt | Throw | Normal | Return =>
          completionCondition.get(targetVar)
        case _ => Some(cond)
      }
    case compoundCond @ CompoundCondition(left, _, right) =>
      (
        rebaseCondition(left, completionCondition),
        rebaseCondition(right, completionCondition),
      ) match {
        case (Some(newLeft), Some(newRight)) =>
          Some(compoundCond.copy(left = newLeft, right = newRight))
        case (None, Some(newRight)) => Some(newRight)
        case (Some(newLeft), None)  => Some(newLeft)
        case (None, None)           => None
      }
    case _ => Some(cond)
  }

  /** bind `x` to the given producer, along with its completion flag
    *
    * A producer that may be abrupt is guarded by a try-catch, so that a thrown
    * value is caught and recorded in the flag of `x`.
    */
  def wrap(
    config: Config,
    x: String,
    expr: Expression,
    ctype: CompletionType,
    isDecl: Boolean,
  ): Config = {
    val flagName = s"${x}_flag"
    val catchVar = s"_${x}_err"
    val step =
      if (isDecl) LetStep(Variable(x), expr)
      else SetStep(Variable(x), expr)
    def aux(flag: String): Step = LetStep(Variable(flagName), EnumLiteral(flag))
    ctype match {
      case MayCompletion =>
        val catchStmts = List(
          SetStep(
            Variable(x),
            ReferenceExpression(Variable(catchVar, None)),
          ),
          aux("abrupt"),
        )
        config :+ WrappedTryCatchStep(
          List(step, aux("normal")).toBlockStep,
          Variable(catchVar),
          Some(catchStmts.toBlockStep),
        )
      case MayNormal     => config :+ step :+ aux("normal")
      case MayAbrupt     => config :+ step :+ aux("abrupt")
      case NotCompletion => config :+ step
    }
  }
}
