package esmeta.es.util
import esmeta.lang.*
import esmeta.lang.BinaryConditionOperator.Eq
import esmeta.lang.util.{UnitWalker => LangUnitWalker, Walker => LangWalker}
import esmeta.spec.*
import esmeta.ty.{NumberTy, ValueTy}

import scala.collection.mutable

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
  ): Option[(Option[Step], CompletionEnv)]
}

class Optimizer(
  val optimizeRules: List[OptimizeRule],
  val transformRules: List[TransformRule],
  val checkedVars: Set[String] = Set.empty,
) {
  import PolyfillInspector.*

  def optimize(
    input: List[Step],
    history: List[Step],
    env: CompletionEnv,
  ): (List[Step], CompletionEnv) = input match {
    case head :: tail =>
      val ctx = OptimizeContext(head, tail, history, env, this)
      optimizeRules.iterator.flatMap(_.apply(ctx)).nextOption() match {
        case Some(result) =>
          optimize(result.remainingInput, result.newHistory, result.newEnv)
        case None =>
          val (newStepOpt, newEnv) = transformStep(head, env)
          newStepOpt match {
            case Some(newStep) =>
              val unwrapped = ValueAccessUnwrapper(env).walk(newStep)
              optimize(tail, unwrapped :: history, newEnv)
            case None =>
              optimize(tail, history, newEnv)
          }
      }
    case Nil => (history.reverse, env)
  }

  def transformStep(
    step: Step,
    env: CompletionEnv,
  ): (Option[Step], CompletionEnv) =
    transformRules.iterator
      .flatMap(_.apply(step, env, this))
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
      val (optimizedBody, _) = optimize(body :: Nil, Nil, env)
      (
        AbstractClosureExpression(params, captured, optimizedBody.toBlockStep),
        None,
      )
    case ReferenceExpression(Variable(name, _)) =>
      (expr, env.getType(name))
    case _ => (expr, None)
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
            case ReferenceExpression(Variable(v, _)) => result.add(v)
            case _                                   => ()
          }
        case ReturnStep(
              ReturnIfAbruptExpression(
                ReferenceExpression(Variable(name, _)),
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
          ReferenceExpression(Variable(name, _)),
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

object PolyfillInspector {

  def transformHead(algo: Algorithm): Head = {
    algo.head match {
      case ao @ AbstractOperationHead(_, _, params, _) =>
        val unwrapParams = params.flatMap {
          case p @ Param(name, Type(ty), paramKind) if ty.isCompletion =>
            List(
              p.copy(
                name = s"${name}_type",
                ty = Type(ValueTy(number = NumberTy.Int)),
              ),
              p.copy(ty = Type(ValueTy.Top)),
            )
          case x => Some(x)
        }
        ao.copy(params = unwrapParams)
      case x => x
    }
  }

  def transformBody(head: Head, step: Step): Step = {
    val paramCompletion = head match {
      case ao @ AbstractOperationHead(_, _, params, _) =>
        params.filter {
          case p @ Param(name, Type(ty), paramKind) => ty.isCompletion
        }
      case x => List()
    }
    val env = paramCompletion.foldLeft(CompletionEnv())((it, item) =>
      it.withType(item.name, ParameterCompletion),
    )
    val checkedVars = CompletionCheckAnalyzer.analyze(step)
    // println(checkedVars)
    val optimizer = new Optimizer(
      optimizeRules = List(
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
      checkedVars = checkedVars,
    )
    optimizer.optimize(step :: Nil, Nil, env)._1.toBlockStep
  }

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
    boolLiteral: Boolean,
    env: CompletionEnv,
  ): Step = {
    val boolLiteralExpr = if (boolLiteral) TrueLiteral() else FalseLiteral()
    if (!env.isFlagDeclared(flagName))
      LetStep(Variable(flagName, None), boolLiteralExpr)
    else SetStep(Variable(flagName, None), boolLiteralExpr)
  }

  def rebaseCondition(
    cond: Condition,
    completionCondition: Map[String, Condition],
  ): Option[Condition] = cond match {
    case PredicateCondition(
          ReferenceExpression(Variable(targetVar, _)),
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
    val catchStmts = List(
      SetStep(
        Variable(varName, None),
        ReferenceExpression(Variable(catchVar, None)),
      ),
      getHoistedFlagSetting(flagName, true, env),
    )
    WrappedTryCatchStep(
      producer.toBlockStep,
      Variable(catchVar),
      Some(catchStmts.toBlockStep),
    )
  }

  extension (l: List[Step])
    def toBlockStep: Step = l match {
      case (b: BlockStep) :: Nil => b
      case _ => BlockStep(StepBlock(l.map(SubStep(None, _))))
    }
}

// =============================================================================
// TransformRules — step-level transformations (no history access)
// =============================================================================

object LetStepTransform extends TransformRule {
  def apply(step: Step, env: CompletionEnv, optimizer: Optimizer) = step match {
    case LetStep(v @ Variable(name, _), expr) =>
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
  def apply(step: Step, env: CompletionEnv, optimizer: Optimizer) = step match {
    case SetStep(v @ Variable(name, _), expr) =>
      expr match {
        case ReturnIfAbruptExpression(
              ReferenceExpression(Variable(inner, _)),
              false,
            ) if name == inner =>
          // Remove redundant Set x = ! x shorthand
          Some((None, env))
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
  def apply(step: Step, env: CompletionEnv, optimizer: Optimizer) = step match {
    case ret @ ReturnStep(
          ReturnIfAbruptExpression(ReferenceExpression(Variable(name, _)), true),
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
                    ReferenceExpression(Variable(s"${name}_type", None)),
                    Eq,
                    NumberLiteral(1),
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
                    ReferenceExpression(Variable(s"${name}_is_abrupt", None)),
                    Eq,
                    TrueLiteral(),
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
  def apply(step: Step, env: CompletionEnv, optimizer: Optimizer) = step match {
    case ReturnStep(
          InvokeAbstractOperationExpression(
            name,
            ReferenceExpression(Variable(varName, _)) :: Nil,
            _,
          ),
        ) if name == "ThrowCompletion" =>
      Some(
        (Some(TaggedStep(ThrowStep(varName), Map("reason" -> "abrupt"))), env),
      )
    case ReturnStep(ReferenceExpression(Variable(name, _)))
        if env.getType(name).contains(AbruptCompletion) =>
      Some(
        (Some(TaggedStep(ThrowStep(name), Map("reason" -> "abrupt"))), env),
      )
    case _ => None
  }
}

object TaggedStepTransform extends TransformRule {
  import PolyfillInspector.*

  def apply(step: Step, env: CompletionEnv, optimizer: Optimizer) = step match {
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
                ),
              )
          }
        case _ =>
          // Unwrap tagged step and recurse
          Some(optimizer.transformStep(taggedInnerStep, env))
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
  ): (Option[Step], CompletionEnv) = {
    val thenType =
      if (checkType == AbruptCompletion) AbruptCompletion
      else NormalCompletion
    val elseType =
      if (checkType == AbruptCompletion) NormalCompletion
      else AbruptCompletion

    val thenEnv = env.withType(targetVar, thenType)
    val elseEnv = env.withType(targetVar, elseType)

    val newThen =
      optimizer.optimize(thenStep :: Nil, Nil, thenEnv)._1.toBlockStep
    val newElse =
      elseStep.map(e =>
        optimizer.optimize(e :: Nil, Nil, elseEnv)._1.toBlockStep,
      )

    val flagVar = tag.getOrElse("USE_FLAG", s"${targetVar}_is_abrupt")
    rebaseCondition(
      cond,
      Map(
        targetVar -> BinaryCondition(
          ReferenceExpression(Variable(flagVar, None)),
          Eq,
          if (checkType == AbruptCompletion) TrueLiteral()
          else UndefinedLiteral(),
        ),
      ),
    ) match {
      case Some(newCond) =>
        (
          Some(TaggedStep(IfStep(newCond, newThen, newElse, cfg), tag)),
          env,
        )
      // TODO Can we ignore ElseStep? If not, how can we handle it?
      case None =>
        (Some(newThen), env)
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
  ): (Option[Step], CompletionEnv) = {
    val newThen = optimizer.optimize(thenStep :: Nil, Nil, env)._1.toBlockStep
    val newElse =
      elseStep.map(e => optimizer.optimize(e :: Nil, Nil, env)._1.toBlockStep)
    rebaseCondition(cond, Map()) match {
      case Some(newCond) =>
        (
          Some(TaggedStep(IfStep(newCond, newThen, newElse, cfg), tag)),
          env,
        )
      // TODO Can we ignore ElseStep? If not, how can we handle it?
      case None =>
        (Some(newThen), env)
    }
  }
}

object BlockStepTransform extends TransformRule {
  import PolyfillInspector.*

  def apply(step: Step, env: CompletionEnv, optimizer: Optimizer) = step match {
    case BlockStep(stmts) =>
      val (newSteps, newEnv) =
        optimizer.optimize(stmts.steps.map(_.step), Nil, env)
      Some((Some(newSteps.toBlockStep), newEnv))
    case _ => None
  }
}

object IfStepTransform extends TransformRule {
  import PolyfillInspector.*

  def apply(step: Step, env: CompletionEnv, optimizer: Optimizer) = step match {
    case IfStep(cond, t, e, cfg) =>
      val (thenSteps, thenEnv) = optimizer.optimize(t :: Nil, Nil, env)
      val (elseResult, elseEnv) = e match {
        case Some(b) =>
          val (steps, eEnv) = optimizer.optimize(b :: Nil, Nil, env)
          (Some(steps.toBlockStep), eEnv)
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

  def apply(step: Step, env: CompletionEnv, optimizer: Optimizer) = step match {
    case RepeatStep(c, b) =>
      Some(
        (
          Some(
            RepeatStep(c, optimizer.optimize(b :: Nil, Nil, env)._1.toBlockStep),
          ),
          env,
        ),
      )
    case _ => None
  }
}

// =============================================================================
// OptimizeRules — stream-level rules (with history access)
// =============================================================================

object ProducerWrapRule extends OptimizeRule {
  import PolyfillInspector.*

  def apply(ctx: OptimizeContext) = ctx.head match {
    case LetStep(Variable(name, _), expr)
        if ctx.optimizer.checkedVars.contains(name)
        && !ctx.env.isHandled(name)
        && !ctx.env.getType(name).contains(ParameterCompletion) =>
      val (newExpr, typeUpdate) = ctx.optimizer.optimizeExpr(expr, ctx.env)
      typeUpdate match {
        case Some(AbruptCompletion) | Some(NormalCompletion) =>
          None // Known types — let LetStepCompletionRule handle
        case _ =>
          val flagName = s"${name}_is_abrupt"
          val wrapped = wrapProducerOnly(
            List(LetStep(Variable(name, None), newExpr)),
            name,
            s"_${name}_err",
            flagName,
            ctx.env,
          )
          val newEnv = ctx.env
            .withHandled(name)
            .withType(name, UnknownCompletion)
            .withFlag(flagName)
          Some(
            OptimizeResult(
              ctx.tail,
              ValueAccessUnwrapper(ctx.env).walk(wrapped) :: ctx.history,
              newEnv,
            ),
          )
      }
    case SetStep(Variable(name, _), expr)
        if ctx.optimizer.checkedVars.contains(name)
        // && !ctx.env.isHandled(name)
        && !ctx.env.getType(name).contains(ParameterCompletion) =>
      val (newExpr, typeUpdate) = ctx.optimizer.optimizeExpr(expr, ctx.env)
      typeUpdate match {
        case Some(AbruptCompletion) | Some(NormalCompletion) =>
          None // Known types — let existing rules handle
        case _ =>
          val flagName = s"${name}_is_abrupt"
          val wrapped = wrapProducerOnly(
            List(SetStep(Variable(name, None), newExpr)),
            name,
            s"_${name}_err",
            flagName,
            ctx.env,
          )
          val newEnv = ctx.env
            .withHandled(name)
            .withType(name, UnknownCompletion)
            .withFlag(flagName)
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

      val transformStep = (ty: CompletionType) =>
        InvokeShorthandStep(
          name,
          List(
            ty match {
              case NormalCompletion => NumberLiteral(0)
              case AbruptCompletion => NumberLiteral(1)
              case ReturnCompletion => NumberLiteral(2)
              case _ =>
                throw RuntimeException(
                  "Cannot convert completion type into literal",
                )
            },
            ReferenceExpression(Variable(s"$targetVar")),
          ) ++ args.drop(1),
        )

      val checkCondition = IfStep(
        BinaryCondition(
          ReferenceExpression(Variable(s"${targetVar}_is_abrupt", None)),
          Eq,
          TrueLiteral(),
        ),
        transformStep(AbruptCompletion),
        Some(transformStep(NormalCompletion)),
      )

      if (ctx.env.isHandled(targetVar)) {
        // Already handled — emit flag-based check
        Some(
          OptimizeResult(
            ctx.tail,
            checkCondition :: ctx.history,
            ctx.env.dropType(targetVar),
          ),
        )
      } else if (ctx.env.getType(targetVar).contains(ParameterCompletion)) {
        // Parameter — use _type variable
        val transformedStep = InvokeShorthandStep(
          name,
          List(
            ReferenceExpression(Variable(s"${targetVar}_type")),
            ReferenceExpression(Variable(s"$targetVar")),
          ) ++ args.drop(1),
        )
        Some(
          OptimizeResult(
            ctx.tail,
            transformedStep :: ctx.history,
            ctx.env.dropType(targetVar),
          ),
        )
      } else {
        // Flag already set by LetStepCompletionRule — use flag-based check
        Some(
          OptimizeResult(
            ctx.tail,
            checkCondition :: ctx.history,
            ctx.env.dropType(targetVar),
          ),
        )
      }
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
      else
        throw RuntimeException(
          s"Unhandled completion check for '$targetVar'",
        )
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
      ) match {
        case (Some(optimizedThen), newEnv) =>
          OptimizeResult(ctx.tail, optimizedThen :: ctx.history, newEnv)
        case (None, newEnv) => OptimizeResult(ctx.tail, ctx.history, newEnv)
      }
    } else {
      val flagName = s"${targetVar}_is_abrupt"
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
      ) match {
        case (Some(optimizedCheck), _) =>
          val continuationEnv =
            if (isTerminal(ifStep.thenStep) && ifStep.elseStep.isEmpty) {
              val oppositeType =
                if (checkType == AbruptCompletion) NormalCompletion
                else AbruptCompletion
              ctx.env.withType(targetVar, oppositeType)
            } else ctx.env
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
    val checkTypeLiteral = NumberLiteral(
      checkType match {
        case NormalCompletion => 0
        case AbruptCompletion => 1
        case ReturnCompletion => 2
        case _                => -1
      },
    )
    val newCond = rebaseCondition(
      ifStep.cond,
      Map(
        targetVar -> BinaryCondition(
          ReferenceExpression(Variable(s"${targetVar}_type", None)),
          Eq,
          checkTypeLiteral,
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
      )
      ._1
      .toBlockStep

    val newElseStep = ifStep.elseStep.map(it =>
      ctx.optimizer
        .optimize(
          it :: Nil,
          Nil,
          ctx.env.withType(targetVar, checkType),
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
    case step @ LetStep(Variable(name, _), _) =>
      val (unwrappedLetStep, newEnv) =
        ctx.optimizer.transformStep(step, ctx.env)
      unwrappedLetStep match {
        case Some(newLetStep) =>
          if (newEnv.getType(name).contains(AbruptCompletion)) {
            Some(
              OptimizeResult(
                ctx.tail,
                getHoistedFlagSetting(
                  s"${name}_is_abrupt",
                  true,
                  newEnv,
                ) :: ValueAccessUnwrapper(ctx.env)
                  .walk(newLetStep) :: ctx.history,
                newEnv.withFlag(s"${name}_is_abrupt"),
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
    case ReferenceExpression(Variable(x, _)) => x
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
    case ReferenceExpression(Access(Variable(varName, _), "Value", _, _)) =>
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
        case x @ ReferenceExpression(v @ Variable(targetVar, nt))
            if nt.isEmpty =>
          env.getType(targetVar) match {
            case Some(AbruptCompletion) =>
              List(NumberLiteral(1), x.copy(v.copy(nt = Some("comp_split"))))
            case Some(NormalCompletion) =>
              List(NumberLiteral(0), x.copy(v.copy(nt = Some("comp_split"))))
            case Some(ReturnCompletion) =>
              List(NumberLiteral(2), x.copy(v.copy(nt = Some("comp_split"))))
            case Some(UnknownCompletion) => Some(x)
            case _                       => Some(x)
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
            case "NormalCompletion" => List(NumberLiteral(0), innerArgs.head)
            case "ThrowCompletion" | "AbruptCompletion" =>
              List(NumberLiteral(1), innerArgs.head)
            case "ReturnCompletion" => List(NumberLiteral(2), innerArgs.head)
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
