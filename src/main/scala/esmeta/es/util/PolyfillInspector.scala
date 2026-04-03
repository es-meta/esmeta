package esmeta.es.util
import esmeta.lang.*
import esmeta.lang.BinaryConditionOperator.Eq
import esmeta.lang.IfStep.ElseConfig
import esmeta.lang.RepeatStep.LoopCondition.{NoCondition, Until, While}
import esmeta.lang.util.{UnitWalker as LangUnitWalker, Walker as LangWalker}
import esmeta.spec.*
import esmeta.ty.{NumberTy, ValueTy}
import org.jsoup.nodes.Element

import scala.collection.mutable

extension (l: List[Step])
  def toBlockStep: Step = l match {
    case (b: BlockStep) :: Nil => b
    case _ =>
      BlockStep(StepBlock(l.flatMap {
        case BlockStep(StepBlock(steps)) => steps
        case x                           => List(SubStep(None, x))
      }))
  }

// =============================================================================
// Completion Types
// =============================================================================

sealed trait CompletionType {
  def toTag: String = this match {
    case NormalCompletion => "normal"
    case AbruptCompletion => "abrupt"
    case other =>
      throw RuntimeException(s"Unexpected completion type in tag: $other")
  }
}
object CompletionType {
  def fromTag(s: String): CompletionType = s match {
    case "normal" => NormalCompletion
    case "abrupt" => AbruptCompletion
    case other    => throw RuntimeException(s"Unknown completion tag: $other")
  }
}
case object NormalCompletion extends CompletionType
case object AbruptCompletion extends CompletionType
case object ReturnCompletion extends CompletionType
case object ParameterCompletion extends CompletionType
case object ResolvedParameterCompletion extends CompletionType
case object UnknownCompletion extends CompletionType

// =============================================================================
// Completion Environment
// =============================================================================

case class CompletionEnv(
  types: Map[String, CompletionType] = Map.empty,
  handled: Set[String] = Set.empty,
  declaredFlag: Set[String] = Set.empty,
) {
  def isFlagDeclared(name: String): Boolean = declaredFlag.contains(name)
  def withFlag(name: String): CompletionEnv =
    copy(declaredFlag = declaredFlag + name)
  def withType(name: String, ty: CompletionType): CompletionEnv =
    copy(types = types + (name -> ty))
  def dropType(name: String): CompletionEnv =
    copy(types = types.removed(name))
  def withHandled(name: String): CompletionEnv =
    copy(handled = handled + name)
  def dropHandled(name: String): CompletionEnv = copy(handled = handled - name)
  def isHandled(name: String): Boolean = handled.contains(name)
  def getType(name: String): Option[CompletionType] = types.get(name)
  def merge(other: CompletionEnv): CompletionEnv = {
    val mergedTypes = (types.keySet ++ other.types.keySet).map { key =>
      (types.get(key), other.types.get(key)) match {
        case (Some(a), Some(b)) if a == b => key -> a
        case (Some(_), Some(_))           => key -> UnknownCompletion
        case (Some(a), None)              => key -> a
        case (None, Some(b))              => key -> b
        case _ => throw RuntimeException("unreachable")
      }
    }.toMap
    val mergedHandled = handled.intersect(other.handled)
    val mergedFlag = declaredFlag ++ other.declaredFlag
    CompletionEnv(mergedTypes, mergedHandled, mergedFlag)
  }
}

// =============================================================================
// Rule Traits and Dispatcher
// =============================================================================

case class OptimizeContext(
  head: Step,
  tail: List[Step],
  history: List[Step],
  env: CompletionEnv,
  optimizer: Optimizer,
  checkedVars: Set[String],
)

case class OptimizeResult(
  remainingInput: List[Step],
  newHistory: List[Step],
  newEnv: CompletionEnv,
)

trait OptimizeRule {
  def apply(ctx: OptimizeContext): Option[OptimizeResult]
}

trait TransformRule {
  def apply(
    step: Step,
    env: CompletionEnv,
    optimizer: Optimizer,
    checkedVars: Set[String],
  ): Option[(Option[Step], CompletionEnv)]
}

class Optimizer(
  val optimizeRules: List[OptimizeRule],
  val transformRules: List[TransformRule],
  val algos: List[Algorithm],
) {
  import PolyfillInspector.*

  def optimize(
    input: List[Step],
    history: List[Step],
    env: CompletionEnv,
    checkedVars: Set[String],
  ): (List[Step], CompletionEnv) = input match {
    case head :: tail =>
      val ctx = OptimizeContext(head, tail, history, env, this, checkedVars)
      optimizeRules.iterator.flatMap(_.apply(ctx)).nextOption() match {
        case Some(result) =>
          optimize(
            result.remainingInput,
            result.newHistory,
            result.newEnv,
            checkedVars,
          )
        case None =>
          val (newStepOpt, newEnv) = transformStep(head, env, checkedVars)
          newStepOpt match {
            case Some(newStep) =>
              val unwrapped = ValueAccessUnwrapper(env).walk(newStep)
              optimize(tail, unwrapped :: history, newEnv, checkedVars)
            case None =>
              optimize(tail, history, newEnv, checkedVars)
          }
      }
    case Nil => (history.reverse, env)
  }

  def transformStep(
    step: Step,
    env: CompletionEnv,
    checkedVars: Set[String],
  ): (Option[Step], CompletionEnv) =
    transformRules.iterator
      .flatMap(_.apply(step, env, this, checkedVars))
      .nextOption()
      .getOrElse((Some(step), env))

  def optimizeExpr(
    expr: Expression,
    env: CompletionEnv,
  ): (Expression, Option[CompletionType]) = expr match {
    case InvokeAbstractOperationExpression("Completion", args, _) =>
      (args.head, Some(UnknownCompletion))
    case InvokeAbstractOperationExpression("NormalCompletion", args, _) =>
      (args.head, Some(NormalCompletion))
    case InvokeAbstractOperationExpression("ThrowCompletion", args, _) =>
      (args.head, Some(AbruptCompletion))
    case InvokeAbstractOperationExpression("AbruptCompletion", args, _) =>
      (args.head, Some(AbruptCompletion))
    case AbstractClosureExpression(params, captured, body) =>
      val closureCheckedVars = CompletionCheckAnalyzer.analyze(body)
      val (optimizedBody, _) =
        optimize(body :: Nil, Nil, env, closureCheckedVars)
      (
        AbstractClosureExpression(params, captured, optimizedBody.toBlockStep),
        None,
      )
    case ReferenceExpression(Variable(name, _, _, _)) =>
      (expr, env.getType(name))
    case ReturnIfAbruptExpression(expr, _) => (expr, None)
    case _                                 => (expr, None)
  }
}

// =============================================================================
// Pre-Analysis: Completion Check Analyzer
// =============================================================================

object CompletionCheckAnalyzer {
  def analyze(step: Step): Set[String] = {
    val result = mutable.Set[String]()
    new LangUnitWalker {
      override def walk(step: Step): Unit = step match {
        case InvokeShorthandStep(name, args) if name.contains("IfAbrupt") =>
          args.head match {
            case ReferenceExpression(Variable(v, _, _, _)) => result.add(v)
            case _                                         => ()
          }
        case ReturnStep(
              ReturnIfAbruptExpression(
                ReferenceExpression(Variable(name, _, _, _)),
                true,
              ),
            ) =>
          result.add(name)
        case IfStep(cond, _, _, _) =>
          extractCheckedVars(cond).foreach(result.add)
          super.walk(step)
        case _ => super.walk(step)
      }
    }.walk(step)
    result.toSet
  }

  private def extractCheckedVars(cond: Condition): List[String] = cond match {
    case PredicateCondition(
          ReferenceExpression(Variable(name, _, _, _)),
          _,
          op,
        ) =>
      import PredicateConditionOperator.*
      op match {
        case Abrupt | Throw | Normal | Return => List(name)
        case _                                => Nil
      }
    case CompoundCondition(left, _, right) =>
      extractCheckedVars(left) ++ extractCheckedVars(right)
    case _ => Nil
  }
}

// =============================================================================
// Entry Point and Utilities
// =============================================================================

class PolyfillInspector(algos: List[Algorithm]) {
  import PolyfillInspector.*

  // Concept optimizer: wrap all completions, always emit kind check on return.
  // - ShorthandInliningRule handles `? x` (ReturnIfAbrupt) by inlining
  // - CompletionCheckRule rewrites explicit "if x is abrupt" spec checks
  // - No ReturnIfAbruptTransform: subsumed by ShorthandInliningRule
  private val simpleOptimizer = new Optimizer(
    optimizeRules = List(
      ShorthandInliningRule,
      XRefInliningRule,
      ProducerWrapRule,
      CompletionCheckRule,
      LetStepCompletionRule,
    ),
    transformRules = List(
      LetStepTransform,
      SetStepTransform,
      ReturnThrowTransform,
      TaggedStepTransform,
      IfStepTransform,
      RecurseTransformRule,
    ),
    algos = algos,
  )

  private val baseOptimizer = new Optimizer(
    optimizeRules = List(
      ShorthandInliningRule,
      ProducerWrapRule,
      IfAbruptRule,
      CompletionCheckRule,
      LetStepCompletionRule,
    ),
    transformRules = List(
      LetStepTransform,
      SetStepTransform,
      ReturnIfAbruptTransform,
      ReturnThrowTransform,
      TaggedStepTransform,
      BlockStepTransform,
      IfStepTransform,
      RepeatStepTransform,
    ),
    algos = algos,
  )
  private val secondPassOptimizer = new Optimizer(
    optimizeRules = List(
      TryCatchOptimizationRule,
    ),
    transformRules = List(
      RecurseTransformRule,
    ),
    algos = algos,
  )

  private val optimizerPass = List(simpleOptimizer) // , secondPassOptimizer)

  def transformHead(head: Head): Head = {
    head match {
      case ao @ AbstractOperationHead(_, _, params, _) =>
        val unwrapParams = params.flatMap {
          case p @ Param(name, Type(ty), paramKind) if ty.isCompletion =>
            List(
              p.copy(
                name = s"${name}_kind",
                ty = Type(ValueTy.Top),
              ),
              p.copy(ty = Type(ValueTy.Top)),
            )
          case x => Some(x)
        }
        ao.copy(params = unwrapParams)
      case x => x
    }
  }

  def transformBody(head: Head, body: Step): Step = {
    val completionParams = head match {
      case AbstractOperationHead(_, _, params, _) =>
        params.filter {
          case Param(name, Type(ty), paramKind) => ty.isCompletion
        }
      case x => List()
    }
    val env = completionParams.foldLeft(CompletionEnv())((it, item) =>
      it.withType(item.name, ParameterCompletion),
    )
    val checkedVars = CompletionCheckAnalyzer.analyze(body)
    optimizerPass
      .foldLeft((body :: Nil, env)) { (params, optimizer) =>
        optimizer.optimize(params._1, Nil, params._2, checkedVars)
      }
      ._1
      .toBlockStep
  }
}

object PolyfillInspector {

  // ---------------------------------------------------------------------------
  // Utility methods shared across rules
  // ---------------------------------------------------------------------------

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
  ): TaggedStep =
    step match {
      case TaggedStep(realStep, existingTag) =>
        TaggedStep(realStep, existingTag + (name -> value))
      case x => TaggedStep(x, Map(name -> value))
    }

  def getHoistedFlagSetting(
    flagName: String,
    kindValue: String, // "abrupt" or "normal"
    env: CompletionEnv,
  ): Step = {
    val kindLiteralExpr = EnumLiteral(kindValue)
    // if (!env.isFlagDeclared(flagName))
    LetStep(Variable(flagName, None), kindLiteralExpr)
    // else SetStep(Variable(flagName, None), kindLiteralExpr)
  }

  def rebaseCondition(
    cond: Condition,
    completionCondition: Map[String, Condition],
  ): Option[Condition] = cond match {
    case PredicateCondition(
          ReferenceExpression(Variable(targetVar, _, _, _)),
          _,
          op,
        ) =>
      import PredicateConditionOperator.*
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

  def wrapProducerOnly(
    producer: List[Step],
    varName: String,
    catchVar: String,
    flagName: String,
    env: CompletionEnv,
  ): Step = {
    // env is expected to already have flagName declared (withFlag), so SetStep is used here
    val catchStmts = List(
      SetStep(
        Variable(varName, None),
        ReferenceExpression(Variable(catchVar, None)),
      ),
      getHoistedFlagSetting(flagName, "abrupt", env),
    )
    WrappedTryCatchStep(
      producer.toBlockStep,
      Variable(catchVar),
      Some(catchStmts.toBlockStep),
    )
  }
}

// =============================================================================
// TransformRules — step-level transformations (no history access)
// =============================================================================

object LetStepTransform extends TransformRule {
  def apply(
    step: Step,
    env: CompletionEnv,
    optimizer: Optimizer,
    checkedVars: Set[String],
  ) = step match {
    case LetStep(v @ Variable(name, _, _, _), expr) =>
      val (newExpr, typeUpdate) = optimizer.optimizeExpr(expr, env)
      if (!env.getType(name).contains(NormalCompletion))
        Some(
          (
            Some(LetStep(v, newExpr)),
            typeUpdate
              .map(t => env.withType(name, t).withHandled(name))
              .getOrElse(env),
          ),
        )
      else Some((Some(LetStep(v, newExpr)), env.withHandled(name)))
    case _ => None
  }
}

object SetStepTransform extends TransformRule {
  def apply(
    step: Step,
    env: CompletionEnv,
    optimizer: Optimizer,
    checkedVars: Set[String],
  ) = step match {
    case SetStep(v @ Variable(name, _, _, _), expr) =>
      expr match {
        case ReturnIfAbruptExpression(
              ReferenceExpression(Variable(inner, _, _, _)),
              false,
            ) if name == inner =>
          // Remove Remove redundant Set x = ! x shorthand
          Some((Some(step), env.dropType(name)))
        case _ =>
          val (newExpr, typeUpdate) = optimizer.optimizeExpr(expr, env)
          env.getType(name) match {
            case _ if typeUpdate.contains(UnknownCompletion) =>
              Some(
                (
                  Some(SetStep(v, newExpr)),
                  typeUpdate
                    .map(t => env.withType(name, t))
                    .getOrElse(env)
                    .dropHandled(name),
                ),
              )
            case Some(NormalCompletion) =>
              Some((Some(SetStep(v, newExpr)), env))
            case Some(_) =>
              Some(
                (
                  Some(SetStep(v, newExpr)),
                  typeUpdate.map(t => env.withType(name, t)).getOrElse(env),
                ),
              )
            case None => Some((Some(SetStep(v, newExpr)), env))
          }
      }
    case _ => None
  }
}

object ReturnIfAbruptTransform extends TransformRule {
  def apply(
    step: Step,
    env: CompletionEnv,
    optimizer: Optimizer,
    checkedVars: Set[String],
  ) = step match {
    case ret @ ReturnStep(
          ReturnIfAbruptExpression(
            ReferenceExpression(Variable(name, _, _, _)),
            true,
          ),
        ) =>
      env.getType(name) match {
        case Some(AbruptCompletion) =>
          Some(
            (Some(TaggedStep(ThrowStep(name), Map("reason" -> "abrupt"))), env),
          )
        case Some(ParameterCompletion) =>
          Some(
            (
              Some(
                IfStep(
                  BinaryCondition(
                    ReferenceExpression(Variable(s"${name}_kind", None)),
                    Eq,
                    EnumLiteral("abrupt"),
                  ),
                  TaggedStep(ThrowStep(name), Map("reason" -> "abrupt")),
                  Some(ret),
                ),
              ),
              env.withType(name, ResolvedParameterCompletion),
            ),
          )
        case Some(UnknownCompletion) =>
          Some(
            (
              Some(
                IfStep(
                  BinaryCondition(
                    ReferenceExpression(Variable(s"${name}_kind", None)),
                    Eq,
                    EnumLiteral("abrupt"),
                  ),
                  TaggedStep(ThrowStep(name), Map("reason" -> "abrupt")),
                  Some(ret),
                ),
              ),
              env.withType(name, ResolvedParameterCompletion),
            ),
          )
        case _ => Some((Some(ret), env))
      }
    case _ => None
  }
}

object ReturnThrowTransform extends TransformRule {
  def apply(
    step: Step,
    env: CompletionEnv,
    optimizer: Optimizer,
    checkedVars: Set[String],
  ) = step match {
    case ReturnStep(
          InvokeAbstractOperationExpression(
            name,
            ReferenceExpression(Variable(varName, _, _, _)) :: Nil,
            _,
          ),
        ) if name == "ThrowCompletion" =>
      Some(
        (Some(TaggedStep(ThrowStep(varName), Map("reason" -> "abrupt"))), env),
      )
    case ReturnStep(ReferenceExpression(Variable(name, _, _, _)))
        if env.getType(name).contains(AbruptCompletion) =>
      Some(
        (Some(TaggedStep(ThrowStep(name), Map("reason" -> "abrupt"))), env),
      )
    // return ? x — ShorthandInliningRule only covers `? x` as a standalone step;
    // `return ? x` is ReturnStep(ReturnIfAbruptExpression(...)) and needs explicit handling.
    case ReturnStep(
          ReturnIfAbruptExpression(
            ReferenceExpression(Variable(name, _, _, _)),
            true,
          ),
        ) =>
      Some(
        (
          Some(
            IfStep(
              BinaryCondition(
                ReferenceExpression(Variable(s"${name}_kind", None)),
                Eq,
                EnumLiteral("abrupt"),
              ),
              TaggedStep(ThrowStep(name), Map("reason" -> "abrupt")),
              Some(ReturnStep(ReferenceExpression(Variable(name, None)))),
            ),
          ),
          env,
        ),
      )
    case ret @ ReturnStep(ReferenceExpression(Variable(name, _, _, _)))
        if env.getType(name).isDefined =>
      Some(
        (
          Some(
            IfStep(
              BinaryCondition(
                ReferenceExpression(Variable(s"${name}_kind", None)),
                Eq,
                EnumLiteral("abrupt"),
              ),
              TaggedStep(ThrowStep(name), Map("reason" -> "abrupt")),
              Some(ret),
            ),
          ),
          env,
        ),
      )
    case _ => None
  }
}

object TaggedStepTransform extends TransformRule {
  import PolyfillInspector.*

  def apply(
    step: Step,
    env: CompletionEnv,
    optimizer: Optimizer,
    checkedVars: Set[String],
  ) = step match {
    case TaggedStep(taggedInnerStep, tag) =>
      taggedInnerStep match {
        case IfStep(cond, thenStep, elseStep, cfg) =>
          val targetVarOpt = tag.get("TARGET_VAR")
          val checkTypeOpt = tag.get("TYPE").map(CompletionType.fromTag)

          (targetVarOpt, checkTypeOpt) match {
            case (Some(targetVar), Some(checkType)) =>
              Some(
                handleTaggedCompletion(
                  cond,
                  thenStep,
                  elseStep,
                  cfg,
                  tag,
                  targetVar,
                  checkType,
                  env,
                  optimizer,
                  checkedVars,
                ),
              )
            case _ =>
              Some(
                handleTaggedGeneric(
                  cond,
                  thenStep,
                  elseStep,
                  cfg,
                  tag,
                  env,
                  optimizer,
                  checkedVars,
                ),
              )
          }
        case _ =>
          // Unwrap tagged step and recurse
          Some(optimizer.transformStep(taggedInnerStep, env, checkedVars))
      }
    case _ => None
  }

  private def handleTaggedCompletion(
    cond: Condition,
    thenStep: Step,
    elseStep: Option[Step],
    cfg: IfStep.ElseConfig,
    tag: Map[String, String],
    targetVar: String,
    checkType: CompletionType,
    env: CompletionEnv,
    optimizer: Optimizer,
    checkedVars: Set[String],
  ): (Option[Step], CompletionEnv) = {
    val thenType =
      if (checkType == AbruptCompletion) AbruptCompletion
      else NormalCompletion
    val elseType =
      if (checkType == AbruptCompletion) NormalCompletion
      else AbruptCompletion

    val thenEnv = env.withType(targetVar, thenType)
    val elseEnv = env.withType(targetVar, elseType)

    val (thenSteps, thenOptEnv) =
      optimizer.optimize(thenStep :: Nil, Nil, thenEnv, checkedVars)
    val newThen = thenSteps.toBlockStep
    val (newElse, elseOptEnv) = elseStep match {
      case Some(e) =>
        val (steps, eEnv) =
          optimizer.optimize(e :: Nil, Nil, elseEnv, checkedVars)
        (Some(steps.toBlockStep), eEnv)
      case None => (None, elseEnv)
    }
    val mergedEnv = (isTerminal(thenStep), elseStep.map(isTerminal)) match {
      case (true, Some(false)) => elseOptEnv
      case (false, Some(true)) => thenOptEnv
      case _                   => thenOptEnv.merge(elseOptEnv)
    }

    val flagVar = tag.getOrElse("USE_FLAG", s"${targetVar}_kind")
    rebaseCondition(
      cond,
      Map(
        targetVar -> BinaryCondition(
          ReferenceExpression(Variable(flagVar, None)),
          Eq,
          if (checkType == AbruptCompletion) EnumLiteral("abrupt")
          else EnumLiteral("normal"),
        ),
      ),
    ) match {
      case Some(newCond) =>
        (
          Some(TaggedStep(IfStep(newCond, newThen, newElse, cfg), tag)),
          mergedEnv,
        )
      // TODO Can we ignore ElseStep? If not, how can we handle it?
      case None =>
        (Some(newThen), mergedEnv)
    }
  }

  private def handleTaggedGeneric(
    cond: Condition,
    thenStep: Step,
    elseStep: Option[Step],
    cfg: IfStep.ElseConfig,
    tag: Map[String, String],
    env: CompletionEnv,
    optimizer: Optimizer,
    checkedVars: Set[String],
  ): (Option[Step], CompletionEnv) = {
    val (thenSteps, thenOptEnv) =
      optimizer.optimize(thenStep :: Nil, Nil, env, checkedVars)
    val newThen = thenSteps.toBlockStep
    val (newElse, elseOptEnv) = elseStep match {
      case Some(e) =>
        val (steps, eEnv) = optimizer.optimize(e :: Nil, Nil, env, checkedVars)
        (Some(steps.toBlockStep), eEnv)
      case None => (None, env)
    }
    val mergedEnv = (isTerminal(thenStep), elseStep.map(isTerminal)) match {
      case (true, Some(false) | None) => elseOptEnv
      case (false, Some(true))        => thenOptEnv
      case _                          => thenOptEnv.merge(elseOptEnv)
    }
    rebaseCondition(cond, Map()) match {
      case Some(newCond) =>
        (
          Some(TaggedStep(IfStep(newCond, newThen, newElse, cfg), tag)),
          mergedEnv,
        )
      // TODO Can we ignore ElseStep? If not, how can we handle it?
      case None =>
        (Some(newThen), mergedEnv)
    }
  }
}

object BlockStepTransform extends TransformRule {
  import PolyfillInspector.*

  def apply(
    step: Step,
    env: CompletionEnv,
    optimizer: Optimizer,
    checkedVars: Set[String],
  ) = step match {
    case BlockStep(stmts) =>
      val (newSteps, newEnv) =
        optimizer.optimize(stmts.steps.map(_.step), Nil, env, checkedVars)
      if (newSteps.isEmpty) Some((None, newEnv))
      else Some((Some(newSteps.toBlockStep), newEnv))
    case _ => None
  }
}

object IfStepTransform extends TransformRule {
  import PolyfillInspector.*

  def apply(
    step: Step,
    env: CompletionEnv,
    optimizer: Optimizer,
    checkedVars: Set[String],
  ) = step match {
    case IfStep(cond, t, e, cfg) =>
      val (thenSteps, thenEnv) =
        optimizer.optimize(t :: Nil, Nil, env, checkedVars)
      val (elseResult, elseEnv) = e match {
        case Some(b) =>
          val (steps, eEnv) =
            optimizer.optimize(b :: Nil, Nil, env, checkedVars)
          if (steps.isEmpty) (None, eEnv)
          else (Some(steps.toBlockStep), eEnv)
        case None => (None, env)
      }
      val mergedEnv = thenEnv.merge(elseEnv)
      Some(
        (Some(IfStep(cond, thenSteps.toBlockStep, elseResult, cfg)), mergedEnv),
      )
    case _ => None
  }
}

object RepeatStepTransform extends TransformRule {
  import PolyfillInspector.*

  def apply(
    step: Step,
    env: CompletionEnv,
    optimizer: Optimizer,
    checkedVars: Set[String],
  ) = step match {
    case RepeatStep(c, b) =>
      Some(
        (
          Some(
            RepeatStep(
              c,
              optimizer.optimize(b :: Nil, Nil, env, checkedVars)._1.toBlockStep,
            ),
          ),
          env,
        ),
      )
    case forEachStep @ ForEachStep(ty, variable, expr, forward, body) =>
      val (newExpr, tyUpdate) = optimizer.optimizeExpr(expr, env)
      val newEnv =
        if (tyUpdate.isDefined) env.withType(variable.name, tyUpdate.get)
        else env
      Some(
        Some(
          forEachStep.copy(
            expr = newExpr,
            body = optimizer
              .optimize(body :: Nil, Nil, newEnv, checkedVars)
              ._1
              .toBlockStep,
          ),
        ),
        env,
      )
    case _ => None
  }
}

// Recurse into all container step types and apply optimizer rules to their bodies.
// Use this as a catch-all transform in passes that don't need per-step env tracking.
object RecurseTransformRule extends TransformRule {
  def apply(
    step: Step,
    env: CompletionEnv,
    optimizer: Optimizer,
    checkedVars: Set[String],
  ): Option[(Option[Step], CompletionEnv)] = step match {
    case BlockStep(StepBlock(stmts)) =>
      val (newSteps, newEnv) =
        optimizer.optimize(stmts.map(_.step), Nil, env, checkedVars)
      if (newSteps.isEmpty) Some((None, newEnv))
      else Some((Some(newSteps.toBlockStep), newEnv))

    case IfStep(cond, t, e, cfg) =>
      val (thenSteps, thenEnv) =
        optimizer.optimize(t :: Nil, Nil, env, checkedVars)
      val (elseResult, elseEnv) = e match {
        case Some(b) =>
          val (steps, eEnv) =
            optimizer.optimize(b :: Nil, Nil, env, checkedVars)
          if (steps.isEmpty) (None, eEnv) else (Some(steps.toBlockStep), eEnv)
        case None => (None, env)
      }
      Some(
        (
          Some(IfStep(cond, thenSteps.toBlockStep, elseResult, cfg)),
          thenEnv.merge(elseEnv),
        ),
      )

    case RepeatStep(c, b) =>
      val newBody =
        optimizer.optimize(b :: Nil, Nil, env, checkedVars)._1.toBlockStep
      Some((Some(RepeatStep(c, newBody)), env))

    case s @ ForEachStep(_, _, _, _, body) =>
      val newBody =
        optimizer.optimize(body :: Nil, Nil, env, checkedVars)._1.toBlockStep
      Some((Some(s.copy(body = newBody)), env))

    case s @ ForEachIntegerStep(_, _, _, _, _, _, body) =>
      val newBody =
        optimizer.optimize(body :: Nil, Nil, env, checkedVars)._1.toBlockStep
      Some((Some(s.copy(body = newBody)), env))

    case s @ ForEachOwnPropertyKeyStep(_, _, _, _, _, body) =>
      val newBody =
        optimizer.optimize(body :: Nil, Nil, env, checkedVars)._1.toBlockStep
      Some((Some(s.copy(body = newBody)), env))

    case s @ ForEachParseNodeStep(_, _, body) =>
      val newBody =
        optimizer.optimize(body :: Nil, Nil, env, checkedVars)._1.toBlockStep
      Some((Some(s.copy(body = newBody)), env))

    case WrappedTryCatchStep(tryBlock, catchVar, catchBlock) =>
      val newTry =
        optimizer
          .optimize(tryBlock :: Nil, Nil, env, checkedVars)
          ._1
          .toBlockStep
      val newCatch = catchBlock.map(b =>
        optimizer.optimize(b :: Nil, Nil, env, checkedVars)._1.toBlockStep,
      )
      Some((Some(WrappedTryCatchStep(newTry, catchVar, newCatch)), env))

    case TaggedStep(inner, tag) =>
      val (newInnerOpt, newEnv) =
        optimizer.transformStep(inner, env, checkedVars)
      Some((newInnerOpt.map(TaggedStep(_, tag)), newEnv))

    case _ => None
  }
}

// =============================================================================
// OptimizeRules — stream-level rules (with history access)
// =============================================================================

object ProducerWrapRule extends OptimizeRule {
  import PolyfillInspector.*

  def apply(ctx: OptimizeContext) = ctx.head match {
    case LetStep(Variable(name, _, _, _), expr)
        if ctx.checkedVars.contains(name)
        && !ctx.env.isHandled(name)
        && !ctx.env.getType(name).contains(ParameterCompletion) =>
      val (newExpr, typeUpdate) = ctx.optimizer.optimizeExpr(expr, ctx.env)
      // Always wrap — no optimization for known types.
      typeUpdate match {
        case Some(NormalCompletion) | Some(AbruptCompletion) | None => None
        case _                                                      =>
          // kindDecl goes inside the try block to avoid polluting the outer x_kind
          // when this wrap is nested inside a prior completion check on the same var.
          val flagName = s"${name}_kind"
          val envWithFlag = ctx.env.withFlag(flagName)
          val kindDecl = getHoistedFlagSetting(flagName, "normal", ctx.env)
          val wrapped = wrapProducerOnly(
            List(LetStep(Variable(name, None), newExpr), kindDecl),
            name,
            s"_${name}_err",
            flagName,
            envWithFlag, // already declared → SetStep in catch
          )
          val newEnv = envWithFlag
            .withHandled(name)
            .withType(name, UnknownCompletion)
          Some(
            OptimizeResult(
              ctx.tail,
              ValueAccessUnwrapper(ctx.env).walk(wrapped) :: ctx.history,
              newEnv,
            ),
          )
      }
    case SetStep(
          Variable(name, _, _, _),
          ReturnIfAbruptExpression(expr, false),
        ) =>
      None
    case SetStep(Variable(name, _, _, _), expr)
        if ctx.checkedVars.contains(name)
        && !ctx.env.getType(name).contains(ParameterCompletion) =>
      val (newExpr, typeUpdate) = ctx.optimizer.optimizeExpr(expr, ctx.env)
      // Always wrap — kindDecl inside try to avoid outer x_kind pollution
      if (typeUpdate.isEmpty) { None }
      else {
        val flagName = s"${name}_kind"
        val envWithFlag = ctx.env.withFlag(flagName)
        val kindDecl = getHoistedFlagSetting(flagName, "normal", ctx.env)
        val wrapped = wrapProducerOnly(
          List(SetStep(Variable(name, None), newExpr), kindDecl),
          name,
          s"_${name}_err",
          flagName,
          envWithFlag, // already declared → SetStep in catch
        )
        val newEnv = envWithFlag
          .withHandled(name)
          .withType(name, UnknownCompletion)
        Some(
          OptimizeResult(
            ctx.tail,
            ValueAccessUnwrapper(ctx.env).walk(wrapped) :: ctx.history,
            newEnv,
          ),
        )
      }
    case _ => None
  }
}

object IfAbruptRule extends OptimizeRule {
  def apply(ctx: OptimizeContext) = ctx.head match {
    case InvokeShorthandStep(name, args) if name.contains("IfAbrupt") =>
      val targetVar = args.head
        .asInstanceOf[ReferenceExpression]
        .ref
        .asInstanceOf[Variable]
        .name

      // Always plug in x_kind directly — no numeric conversion
      val kindRef = ReferenceExpression(Variable(s"${targetVar}_kind", None))

      val transformedStep = InvokeShorthandStep(
        name,
        List(kindRef, ReferenceExpression(Variable(s"$targetVar"))) ++ args
          .drop(1),
      )

      Some(
        OptimizeResult(
          ctx.tail,
          transformedStep :: ctx.history,
          ctx.env.dropType(targetVar),
        ),
      )
    case _ => None
  }
}

object CompletionCheckRule extends OptimizeRule {
  import PolyfillInspector.*

  def apply(ctx: OptimizeContext) = ctx.head match {
    case check @ CompletionCheckPattern(checks) =>
      val ifStep = check.asInstanceOf[IfStep]
      val (checkType, targetVar) = checks

      if (ctx.env.isHandled(targetVar))
        Some(handleAlreadyHandled(ctx, check, ifStep, checkType, targetVar))
      else if (ctx.env.getType(targetVar).contains(ParameterCompletion))
        Some(handleParameter(ctx, ifStep, checkType, targetVar))
      else {
        System.err.println(s"Unhandled completion check for '$targetVar'")
        Some(handleAlreadyHandled(ctx, check, ifStep, checkType, targetVar))
      }
    case _ => None
  }

  private def handleAlreadyHandled(
    ctx: OptimizeContext,
    check: Step,
    ifStep: IfStep,
    checkType: CompletionType,
    targetVar: String,
  ): OptimizeResult = {
    val canOmit = ifStep.elseStep.isEmpty &&
      ctx.env.getType(targetVar).contains(NormalCompletion)

    if (canOmit) {
      ctx.optimizer.transformStep(
        ifStep.thenStep,
        ctx.env.withType(targetVar, checkType),
        ctx.checkedVars,
      ) match {
        case (Some(optimizedThen), newEnv) =>
          OptimizeResult(ctx.tail, optimizedThen :: ctx.history, newEnv)
        case (None, newEnv) => OptimizeResult(ctx.tail, ctx.history, newEnv)
      }
    } else {
      val flagName = s"${targetVar}_kind"
      val taggedCheck = annotateStep(
        annotateStep(
          annotateStep(check, "USE_FLAG", flagName),
          "TYPE",
          checkType.toTag,
        ),
        "TARGET_VAR",
        targetVar,
      )
      ctx.optimizer.transformStep(
        taggedCheck,
        ctx.env.withType(targetVar, checkType).withHandled(targetVar),
        ctx.checkedVars,
      ) match {
        case (Some(optimizedCheck), continuationEnv) =>
          OptimizeResult(
            ctx.tail,
            optimizedCheck :: ctx.history,
            continuationEnv,
          )
        case (None, newEnv) => OptimizeResult(ctx.tail, ctx.history, newEnv)
      }
    }
  }

  private def handleParameter(
    ctx: OptimizeContext,
    ifStep: IfStep,
    checkType: CompletionType,
    targetVar: String,
  ): OptimizeResult = {
    val checkKindLiteral = EnumLiteral(checkType.toTag)
    val newCond = rebaseCondition(
      ifStep.cond,
      Map(
        targetVar -> BinaryCondition(
          ReferenceExpression(Variable(s"${targetVar}_kind", None)),
          Eq,
          checkKindLiteral,
        ),
      ),
    )
      .getOrElse(
        throw RuntimeException(
          "Checking completion from parameter cannot be omitted",
        ),
      )

    val isAbruptTerminal = isTerminal(ifStep.thenStep)
    val newThenStep = ctx.optimizer
      .optimize(
        ifStep.thenStep :: Nil,
        Nil,
        ctx.env.withType(targetVar, checkType),
        ctx.checkedVars,
      )
      ._1
      .toBlockStep

    val newElseStep = ifStep.elseStep.map(it =>
      ctx.optimizer
        .optimize(
          it :: Nil,
          Nil,
          ctx.env.withType(targetVar, checkType),
          ctx.checkedVars,
        )
        ._1
        .toBlockStep,
    )

    val newIfStep = ifStep.copy(
      cond = newCond,
      thenStep = newThenStep,
      elseStep = newElseStep,
    )
    val newEnv =
      if (checkType == AbruptCompletion && isAbruptTerminal)
        ctx.env.withType(targetVar, ResolvedParameterCompletion)
      else ctx.env
    OptimizeResult(ctx.tail, newIfStep :: ctx.history, newEnv)
  }
}

object LetStepCompletionRule extends OptimizeRule {
  import PolyfillInspector.*

  def apply(ctx: OptimizeContext) = ctx.head match {
    case step @ LetStep(Variable(name, _, _, _), _) =>
      val (unwrappedLetStep, newEnv) =
        ctx.optimizer.transformStep(step, ctx.env, ctx.checkedVars)
      unwrappedLetStep match {
        case Some(newLetStep) =>
          if (newEnv.getType(name).contains(AbruptCompletion)) {
            Some(
              OptimizeResult(
                ctx.tail,
                getHoistedFlagSetting(
                  s"${name}_kind",
                  "abrupt",
                  newEnv,
                ) :: ValueAccessUnwrapper(ctx.env)
                  .walk(newLetStep) :: ctx.history,
                newEnv.withFlag(s"${name}_kind"),
              ),
            )
          } else
            Some(
              OptimizeResult(
                ctx.tail,
                ValueAccessUnwrapper(ctx.env).walk(newLetStep) :: ctx.history,
                newEnv,
              ),
            )
        case None =>
          ???
          Some(OptimizeResult(ctx.tail, ctx.history, newEnv))
      }
    case _ => None
  }
}

object ShorthandInliningRule extends OptimizeRule {
  import PolyfillInspector.*

  override def apply(ctx: OptimizeContext): Option[OptimizeResult] =
    ctx.head match {
      case InvokeShorthandStep(name, args) =>
        val targetAlgo = ctx.optimizer.algos.find(_.name == name)
        if (targetAlgo.isEmpty) None
        else {
          val targetStep = targetAlgo.get.body
          // TODO: ignore parameter types for now
          val targetParameters = targetAlgo.get.head.originalParams.map(_.name)
          // Parameter matching
          val inlinedStep = (targetParameters zip args).foldLeft(targetStep) {
            (step, paramToArg) =>
              ParameterInlineWalker(paramToArg._1, paramToArg._2).walk(step)
          }
          val (transformedStep, newEnv) =
            ctx.optimizer.transformStep(inlinedStep, ctx.env, ctx.checkedVars)
          Some(
            OptimizeResult(
              ctx.tail,
              transformedStep.getOrElse(
                throw RuntimeException(
                  s"Cannot inline shorthand: ${name} : ${targetStep}",
                ),
              ) :: ctx.history,
              newEnv,
            ),
          )
        }
      case _ => None
    }

  private class ParameterInlineWalker(
    paramName: String,
    replaceWith: Expression,
  ) extends LangWalker {
    override def walk(expr: Expression): Expression = expr match {
      case ReferenceExpression(ref) =>
        ref match {
          case Variable(name, None, _, _) =>
            if (name == paramName) replaceWith else expr
          case x => ReferenceExpression(walk(x))
        }
      case _ => super.walk(expr)
    }

    override def walk(ref: Reference): Reference = ref match {
      case Variable(name, _, _, _) =>
        if (name == paramName) {
          replaceWith.asInstanceOf[ReferenceExpression].ref
        } else ref
      case x => super.walk(x)
    }
  }
}

object XRefInliningRule extends OptimizeRule {

  import PolyfillInspector.*

  override def apply(ctx: OptimizeContext): Option[OptimizeResult] =
    ctx.head match {
      // XRefExpressionOperator.Algo = Let x be the algorithm steps defined in...
      case step @ LetStep(
            Variable(name, _, _, _),
            XRefExpression(XRefExpressionOperator.Algo, id),
          ) =>
        val targetFunction = ctx.optimizer.algos.find(_.name.endsWith(id))
        if (targetFunction.isEmpty) None
        else {
          val func = targetFunction.head
          val extractedHead = func.head.asInstanceOf[BuiltinHead]
          val extractedBody = func.body
          val optimizedClosureBody = ctx.optimizer
            .optimize(
              extractedBody :: Nil,
              Nil,
              CompletionEnv(),
              CompletionCheckAnalyzer.analyze(extractedBody),
            )
            ._1
            .toBlockStep
          val params = extractedHead.params
          val closureExpression = AbstractClosureExpression(
            params.map(it => Variable(it.name, Some("xref_inlined"))),
            List(),
            optimizedClosureBody,
          )
          Some(
            OptimizeResult(
              ctx.tail,
              step.copy(expr = closureExpression) :: ctx.history,
              ctx.env,
            ),
          )
        }
      case _ => None
    }

  extension (elem: Element) {
    def getId: String = {
      if (elem.id != "") elem.id
      else if (elem.parent == null) ""
      else elem.parent.getId
    }
  }
}

object TryCatchOptimizationRule extends OptimizeRule {

  override def apply(ctx: OptimizeContext): Option[OptimizeResult] =
    (ctx.history.headOption, ctx.head) match {
      case (
            Some(
              tryCatchStep @ WrappedTryCatchStep(
                tryBlock,
                Variable(catchVar, nt, _, _),
                catchBlock,
              ),
            ),
            ifStep @ TaggedStep(
              IfStep(cond, thenStep, None, elseConfig),
              tag,
            ),
          ) =>
        val targetVar = tag.getOrElse(
          "TARGET_VAR",
          throw RuntimeException("No TARGET_VAR tag"),
        )

        // Check the separation is needed
        // If the producer and the consumer are neighbor, we can combine them
        if (catchVar != s"_${targetVar}_err") None
        else {
          val completionType = CompletionType.fromTag(
            tag.getOrElse("TYPE", throw RuntimeException("No TYPE tag")),
          )

          if (!cond.isInstanceOf[BinaryCondition]) {
            // For now, let's ignore composite condition
            None
          } else {
            completionType match {
              case NormalCompletion =>
                val newWrappedTryCatch = tryCatchStep.copy(
                  tryBlock = List(tryBlock, thenStep).toBlockStep,
                )
                Some(
                  OptimizeResult(
                    ctx.tail,
                    newWrappedTryCatch :: ctx.history.drop(1),
                    ctx.env,
                  ),
                )
              case AbruptCompletion =>
                val newWrappedTryCatch = tryCatchStep.copy(
                  catchBlock =
                    Some(List(catchBlock, Some(thenStep)).flatten.toBlockStep),
                )
                Some(
                  OptimizeResult(
                    ctx.tail,
                    newWrappedTryCatch :: ctx.history.drop(1),
                    ctx.env,
                  ),
                )
              case ParameterCompletion => None
              case _                   => None
            }
          }
        }
      case _ => None
    }
}

// =============================================================================
// Pattern Matching Helpers
// =============================================================================

private object CompletionCheckPattern {
  def unapply(step: Step): Option[(CompletionType, String)] = step match {
    case IfStep(cond, thenStep, elseStep, config) => traverseCondition(cond)
    case _                                        => None
  }

  private def traverseCondition(
    cond: Condition,
  ): Option[(CompletionType, String)] =
    cond match {
      case PredicateCondition(expr, _, op) =>
        import PredicateConditionOperator.*
        op match {
          case Abrupt | Throw => Some((AbruptCompletion, extractVarName(expr)))
          case Normal         => Some((NormalCompletion, extractVarName(expr)))
          case Return         => Some((ReturnCompletion, extractVarName(expr)))
          case _              => None
        }
      case CompoundCondition(left, op, right) =>
        traverseCondition(left).orElse(traverseCondition(right))
      case _ => None
    }

  private def extractVarName(expr: Expression) = expr match {
    case ReferenceExpression(Variable(x, _, _, _)) => x
    case err =>
      throw RuntimeException(
        s"Expected Reference Expression for extractVarName, but got '${err.toString}'",
      )
  }
}

// =============================================================================
// Value Access Unwrapper
// =============================================================================

private class ValueAccessUnwrapper(env: CompletionEnv) extends LangWalker {

  override def walk(step: Step): Step = step match {
    case WrappedTryCatchStep(tryBlock, catchVar, catchBlock) =>
      WrappedTryCatchStep(
        walk(tryBlock),
        walk(catchVar),
        walkOpt(catchBlock, walk),
      )
    case TaggedStep(innerStep, tag) =>
      TaggedStep(walk(innerStep), tag)
    case _ => super.walk(step)
  }

  override def walk(expr: Expression): Expression = expr match {
    // Unwrap .[[Value]] access on known completion types
    case ReferenceExpression(
          Access(Variable(varName, _, _, _), "Value", _, _),
        ) =>
      env.getType(varName) match {
        case Some(_) =>
          ReferenceExpression(Variable(varName, Some("value_unwrapped")))
        case None => super.walk(expr)
      }
    // Unwrap Completion AO calls
    case completionAO @ InvokeAbstractOperationExpression(name, args, _)
        if name.contains("Completion") =>
      if (args.length > 1)
        throw RuntimeException(
          s"Completion AO Call should contain up to one argument:\n\t$completionAO",
        )
      args.head
    // AO calls with completion argument unpacking
    case aoExpr @ InvokeAbstractOperationExpression(name, args, _) =>
      val newArgs = args.flatMap {
        case x @ ReferenceExpression(v @ Variable(targetVar, nt, _, _))
            if nt.isEmpty =>
          env.getType(targetVar) match {
            case Some(AbruptCompletion) | Some(NormalCompletion) |
                Some(ReturnCompletion) | Some(UnknownCompletion) =>
              // Plug in x_kind directly — no numeric conversion
              List(
                ReferenceExpression(Variable(s"${targetVar}_kind", None)),
                x.copy(v.copy(nt = Some("comp_split"))),
              )
            case _ => Some(x)
          }
        case c @ InvokeAbstractOperationExpression(
              innerCallName,
              innerArgs,
              _,
            ) if innerCallName.contains("Completion") =>
          if (innerArgs.length > 1)
            throw RuntimeException(
              s"Completion AO Call should contain up to one argument:\n\t$c",
            )
          innerCallName match {
            case "NormalCompletion" =>
              List(EnumLiteral("normal"), innerArgs.head)
            case "ThrowCompletion" | "AbruptCompletion" =>
              List(EnumLiteral("abrupt"), innerArgs.head)
            case "ReturnCompletion" =>
              List(EnumLiteral("return"), innerArgs.head)
            case "Completion" =>
              throw RuntimeException(
                s"Cannot unpack the raw completion object: $c",
              )
            case _ => Some(c.copy(args = innerArgs.map(walk)))
          }
        case x => Some(walk(x))
      }
      aoExpr.copy(args = newArgs)
    case _ => super.walk(expr)
  }
}
