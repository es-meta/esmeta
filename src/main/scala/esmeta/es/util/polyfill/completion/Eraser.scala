package esmeta.es.util.polyfill.completion

import esmeta.es.util.polyfill.*
import esmeta.es.util.polyfill.completion.CompletionType.*
import esmeta.es.util.polyfill.completion.rules.completion.*
import esmeta.es.util.polyfill.completion.rules.inlining.*
import esmeta.es.util.polyfill.completion.rules.structural.*
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
class Eraser(algo: Algorithm, algos: List[Algorithm]) {
  val head = algo.head
  val body = algo.body

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
      CompletionCheckTransform,
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
  def transformBody: PolyfillStep = {
    val env = TypeEnv((for {
      param <- head.originalParams
      if param.ty.ty.isCompletion
    } yield param.name -> MayCompletion).toMap)
    rewriter.transformBlock(PolyfillStep.lift(body), Config(env))
  }
}

// =============================================================================
// Utilities shared across rules
// =============================================================================

object Eraser {

  /** replace completion checks in a condition with checks on completion flags,
    * dropping the condition when it becomes vacuous
    */
  def rebaseCondition(
    cond: Condition,
    completionCondition: Map[String, Condition],
  ): Option[Condition] = cond match {
    case PredicateCondition(
          List(ReferenceExpression(Variable(targetVar, _, _, _))),
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
    expr: PolyfillExpr,
    ctype: CompletionType,
    isDecl: Boolean,
  ): Config = {
    import PolyfillStep.*
    val flagName = s"${x}_flag"
    val catchVar = s"_${x}_err"
    val step =
      if (isDecl) Let(Variable(x), expr)
      else Assign(Variable(x), expr)
    def aux(flag: String): PolyfillStep =
      Let(Variable(flagName), PolyfillExpr.LangExpr(EnumLiteral(flag)))
    ctype match {
      case MayCompletion =>
        val catchStmts = List(
          Assign(
            Variable(x),
            PolyfillExpr.LangExpr(
              ReferenceExpression(Variable(catchVar, None)),
            ),
          ),
          aux("abrupt"),
        )
        config :+ Wrapped(
          List(step, aux("normal")).toSteps,
          Variable(catchVar),
          Some(catchStmts.toSteps),
        )
      case MayNormal     => config :+ step :+ aux("normal")
      case MayAbrupt     => config :+ step :+ aux("abrupt")
      case NotCompletion => config :+ step
    }
  }
}

/** rule dispatcher */
class Rewriter(
  val rules: List[EraseRule],
  val algos: List[Algorithm],
) {
  import PolyfillStep.*
  import PolyfillExpr.*

  /** rewrite a step and append the result to the given config */
  def transform(step: PolyfillStep, config: Config): Config =
    rules.iterator
      .flatMap(_(step, config, this))
      .nextOption()
      .getOrElse(config :+ step)

  /** rewrite a nested step in a fresh step buffer, and pack it into a block */
  def transformBlock(step: PolyfillStep, config: Config): PolyfillStep =
    transform(step, config.clear).steps.toSteps

  /** rewrite an expression, along with the completion type it produces */
  def transformExpr(
    expr: PolyfillExpr,
    config: Config,
  ): (PolyfillExpr, CompletionType) = expr match {
    case Invoke("Completion", args, _)       => (args.head, MayCompletion)
    case Invoke("NormalCompletion", args, _) => (args.head, MayNormal)
    case Invoke("ThrowCompletion", args, _)  => (args.head, MayAbrupt)
    case Invoke("AbruptCompletion", args, _) => (args.head, MayAbrupt)
    case Closure(params, captured, body) =>
      (
        Closure(params, captured, transformBlock(body, config)),
        NotCompletion,
      )
    case LangExpr(ReferenceExpression(Variable(name, _, _, _))) =>
      (expr, config(name))
    case LangExpr(ReturnIfAbruptExpression(inner, _)) =>
      (LangExpr(inner), NotCompletion)
    case _ => (expr, NotCompletion)
  }
}

/** completion record erasure */
object CompletionPath {
  def apply(targets: List[Algorithm]): List[PolyfillAlgo] = {
    targets.map { algo =>
      val inspector = new Eraser(algo, targets)
      PolyfillAlgo(algo.name, inspector.transformHead, inspector.transformBody)
    }
  }
}
