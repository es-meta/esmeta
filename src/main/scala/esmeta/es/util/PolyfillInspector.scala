package esmeta.es.util

import esmeta.lang.*
import esmeta.lang.BinaryConditionOperator.Eq
import esmeta.lang.IfStep.ElseConfig
import esmeta.lang.RepeatStep.LoopCondition.{NoCondition, Until, While}
import esmeta.lang.util.{UnitWalker as LangUnitWalker, Walker as LangWalker}
import esmeta.spec.*
import esmeta.util.BaseUtils.*
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
enum CompletionType {
  case MayCompletion
  case MayNormal
  case MayAbrupt
  case NotCompletion

  def toTag: String = this match
    case MayNormal => "normal"
    case MayAbrupt => "abrupt"
    case other     => raise(s"Unexpected completion type in tag: $other")
  def join(that: CompletionType): CompletionType = (this, that) match {
    case _ if this == that  => this
    case (NotCompletion, _) => that
    case (_, NotCompletion) => this
    case _                  => MayCompletion
  }
}
object CompletionType {
  def fromTag(s: String): CompletionType = s match {
    case "normal" => MayNormal
    case "abrupt" => MayAbrupt
    case other    => raise(s"Unknown completion tag: $other")
  }
}
import CompletionType.*

// =============================================================================
// Type Environment
// =============================================================================
case class TypeEnv(
  types: Map[String, CompletionType] = Map.empty,
) {
  def +(pair: (String, CompletionType)): TypeEnv =
    val (name, ty) = pair
    if (ty == NotCompletion) this
    else copy(types = types + (name -> ty))
  def -(name: String): TypeEnv = copy(types = types - name)
  def apply(name: String): CompletionType = types.getOrElse(name, NotCompletion)
  def ++(that: TypeEnv): TypeEnv = TypeEnv(
    (this.types.keySet ++ that.types.keySet).toList.map { key =>
      key -> (this(key) join that(key))
    }.toMap,
  )
}

// =============================================================================
// Contexts
// =============================================================================
case class Context(
  input: List[Step],
  history: List[Step],
  env: TypeEnv,
  optimizer: Optimizer,
  checkedVars: Set[String],
)

trait OptimizeRule {
  def apply(ctx: Context): Option[Context]
}

trait TransformRule {
  def apply(
    step: Step,
    env: TypeEnv,
    optimizer: Optimizer,
    checkedVars: Set[String],
  ): Option[(Option[Step], TypeEnv)]
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
    env: TypeEnv,
    checkedVars: Set[String],
  ): (List[Step], TypeEnv) = input match {
    case head :: tail =>
      val ctx = Context(input, history, env, this, checkedVars)
      optimizeRules.iterator.flatMap(_.apply(ctx)).nextOption() match {
        case Some(result) =>
          optimize(
            result.input,
            result.history,
            result.env,
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
    env: TypeEnv,
    checkedVars: Set[String],
  ): (Option[Step], TypeEnv) =
    transformRules.iterator
      .flatMap(_.apply(step, env, this, checkedVars))
      .nextOption()
      .getOrElse((Some(step), env))

  def optimizeExpr(
    expr: Expression,
    env: TypeEnv,
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
      val closureCheckedVars = CompletionCheckAnalyzer.analyze(body)
      val (optimizedBody, _) =
        optimize(body :: Nil, Nil, env, closureCheckedVars)
      (
        AbstractClosureExpression(params, captured, optimizedBody.toBlockStep),
        NotCompletion,
      )
    case ReferenceExpression(Variable(name, _, _, _)) =>
      (expr, env(name))
    case ReturnIfAbruptExpression(expr, _) => (expr, NotCompletion)
    case _                                 => (expr, NotCompletion)
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

  // Concept optimizer: wrap all completions, always emit flag check on return.
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

  def transformHead(head: Head): Head = {
    head match {
      case ao @ AbstractOperationHead(_, _, params, _) =>
        val unwrapParams = params.flatMap {
          case p @ Param(name, Type(ty), paramKind) if ty.isCompletion =>
            List(
              p.copy(
                name = s"${name}_flag",
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
    val env = completionParams.foldLeft(TypeEnv())((it, item) =>
      it + (item.name -> MayCompletion),
    )
    val checkedVars = CompletionCheckAnalyzer.analyze(body)
    simpleOptimizer.optimize(body :: Nil, Nil, env, checkedVars)._1.toBlockStep
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
    flag: String, // "abrupt" or "normal"
    env: TypeEnv,
  ): Step = LetStep(Variable(flagName, None), EnumLiteral(flag))

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
    env: TypeEnv,
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
    env: TypeEnv,
    optimizer: Optimizer,
    checkedVars: Set[String],
  ) = step match {
    case LetStep(v @ Variable(name, _, _, _), expr) =>
      val (newExpr, typeUpdate) = optimizer.optimizeExpr(expr, env)
      if (env(name) != MayNormal)
        Some(
          Some(LetStep(v, newExpr)),
          env + (name -> typeUpdate),
        )
      else Some(Some(LetStep(v, newExpr)), env)
    case _ => None
  }
}

object SetStepTransform extends TransformRule {
  def apply(
    step: Step,
    env: TypeEnv,
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
          Some(Some(step), env - name)
        case _ =>
          val (newExpr, typeUpdate) = optimizer.optimizeExpr(expr, env)
          env(name) match {
            case _ if typeUpdate == MayCompletion =>
              Some(
                Some(SetStep(v, newExpr)),
                env + (name -> typeUpdate),
              )
            case MayNormal =>
              Some((Some(SetStep(v, newExpr)), env))
            case _ =>
              Some(
                Some(SetStep(v, newExpr)),
                env + (name -> typeUpdate),
              )
          }
      }
    case _ => None
  }
}

object ReturnThrowTransform extends TransformRule {
  def apply(
    step: Step,
    env: TypeEnv,
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
        if env(name) == MayAbrupt =>
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
        Some(
          IfStep(
            BinaryCondition(
              ReferenceExpression(Variable(s"${name}_flag", None)),
              Eq,
              EnumLiteral("abrupt"),
            ),
            TaggedStep(ThrowStep(name), Map("reason" -> "abrupt")),
            Some(ReturnStep(ReferenceExpression(Variable(name, None)))),
          ),
        ),
        env,
      )
    case ret @ ReturnStep(ReferenceExpression(Variable(name, _, _, _)))
        if env(name) != NotCompletion =>
      Some(
        Some(
          IfStep(
            BinaryCondition(
              ReferenceExpression(Variable(s"${name}_flag", None)),
              Eq,
              EnumLiteral("abrupt"),
            ),
            TaggedStep(ThrowStep(name), Map("reason" -> "abrupt")),
            Some(ret),
          ),
        ),
        env,
      )
    case _ => None
  }
}

object TaggedStepTransform extends TransformRule {
  import PolyfillInspector.*

  def apply(
    step: Step,
    env: TypeEnv,
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
    env: TypeEnv,
    optimizer: Optimizer,
    checkedVars: Set[String],
  ): (Option[Step], TypeEnv) = {
    val thenType =
      if (checkType == MayAbrupt) MayAbrupt
      else MayNormal
    val elseType =
      if (checkType == MayAbrupt) MayNormal
      else MayAbrupt

    val thenEnv = env + (targetVar -> thenType)
    val elseEnv = env + (targetVar -> elseType)

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
      case _                   => thenOptEnv ++ elseOptEnv
    }

    val flagVar = tag.getOrElse("USE_FLAG", s"${targetVar}_flag")
    rebaseCondition(
      cond,
      Map(
        targetVar -> BinaryCondition(
          ReferenceExpression(Variable(flagVar, None)),
          Eq,
          if (checkType == MayAbrupt) EnumLiteral("abrupt")
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
    env: TypeEnv,
    optimizer: Optimizer,
    checkedVars: Set[String],
  ): (Option[Step], TypeEnv) = {
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
      case _                          => thenOptEnv ++ elseOptEnv
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

object IfStepTransform extends TransformRule {
  import PolyfillInspector.*

  def apply(
    step: Step,
    env: TypeEnv,
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
      val mergedEnv = thenEnv ++ elseEnv
      Some(
        (Some(IfStep(cond, thenSteps.toBlockStep, elseResult, cfg)), mergedEnv),
      )
    case _ => None
  }
}

// Recurse into all container step types and apply optimizer rules to their bodies.
// Use this as a catch-all transform in passes that don't need per-step env tracking.
object RecurseTransformRule extends TransformRule {
  def apply(
    step: Step,
    env: TypeEnv,
    optimizer: Optimizer,
    checkedVars: Set[String],
  ): Option[(Option[Step], TypeEnv)] = step match {
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
        Some(IfStep(cond, thenSteps.toBlockStep, elseResult, cfg)),
        thenEnv ++ elseEnv,
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

  def apply(ctx: Context): Option[Context] = ctx.input match {
    case LetStep(Variable(name, _, _, _), expr) :: tail
        if ctx.checkedVars.contains(name)
        && ctx.env(name) != MayCompletion =>
      val (newExpr, typeUpdate) = ctx.optimizer.optimizeExpr(expr, ctx.env)
      // Always wrap — no optimization for known types.
      typeUpdate match {
        case MayNormal | MayAbrupt | NotCompletion => None
        case _ =>
          val flagName = s"${name}_flag"
          val env = ctx.env
          val flagDecl = getHoistedFlagSetting(flagName, "normal", ctx.env)
          val wrapped = wrapProducerOnly(
            List(LetStep(Variable(name, None), newExpr), flagDecl),
            name,
            s"_${name}_err",
            flagName,
            env, // already declared → SetStep in catch
          )
          val newEnv = env + (name -> MayCompletion)
          Some(
            Context(
              tail,
              ValueAccessUnwrapper(ctx.env).walk(wrapped) :: ctx.history,
              newEnv,
              ctx.optimizer,
              ctx.checkedVars,
            ),
          )
      }
    case SetStep(
          Variable(name, _, _, _),
          ReturnIfAbruptExpression(expr, false),
        ) :: _ =>
      None
    case SetStep(Variable(name, _, _, _), expr) :: tail
        if ctx.checkedVars.contains(name)
        && ctx.env(name) != MayCompletion =>
      val (newExpr, typeUpdate) = ctx.optimizer.optimizeExpr(expr, ctx.env)
      if (typeUpdate == NotCompletion) None
      else {
        val flagName = s"${name}_flag"
        val env = ctx.env
        val flagDecl = getHoistedFlagSetting(flagName, "normal", ctx.env)
        val wrapped = wrapProducerOnly(
          List(SetStep(Variable(name, None), newExpr), flagDecl),
          name,
          s"_${name}_err",
          flagName,
          env, // already declared → SetStep in catch
        )
        val newEnv = env + (name -> MayCompletion)
        Some(
          Context(
            tail,
            ValueAccessUnwrapper(ctx.env).walk(wrapped) :: ctx.history,
            newEnv,
            ctx.optimizer,
            ctx.checkedVars,
          ),
        )
      }
    case _ => None
  }
}

object CompletionCheckRule extends OptimizeRule {
  import PolyfillInspector.*

  def apply(ctx: Context): Option[Context] = ctx.input match {
    case (check @ CompletionCheckPattern(checks)) :: tail =>
      val ifStep = check.asInstanceOf[IfStep]
      val (checkType, targetVar) = checks
      val canOmit = ifStep.elseStep.isEmpty &&
        ctx.env(targetVar) == MayNormal

      val result = if (canOmit) {
        ctx.optimizer.transformStep(
          ifStep.thenStep,
          ctx.env + (targetVar -> checkType),
          ctx.checkedVars,
        ) match {
          case (Some(optimizedThen), newEnv) =>
            ctx.copy(ctx.input.tail, optimizedThen :: ctx.history, newEnv)
          case (None, newEnv) => ctx.copy(ctx.input.tail, ctx.history, newEnv)
        }
      } else {
        val flagName = s"${targetVar}_flag"
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
          ctx.env + (targetVar -> checkType),
          ctx.checkedVars,
        ) match {
          case (Some(optimizedCheck), continuationEnv) =>
            ctx.copy(
              ctx.input.tail,
              optimizedCheck :: ctx.history,
              continuationEnv,
            )
          case (None, newEnv) => ctx.copy(ctx.input.tail, ctx.history, newEnv)
        }
      }
      Some(result)
    case _ => None
  }

  private def handleParameter(
    ctx: Context,
    ifStep: IfStep,
    checkType: CompletionType,
    targetVar: String,
  ): Context = {
    val checkFlagLiteral = EnumLiteral(checkType.toTag)
    val newCond = rebaseCondition(
      ifStep.cond,
      Map(
        targetVar -> BinaryCondition(
          ReferenceExpression(Variable(s"${targetVar}_flag", None)),
          Eq,
          checkFlagLiteral,
        ),
      ),
    )
      .getOrElse(raise("Checking completion from parameter cannot be omitted"))

    val isAbruptTerminal = isTerminal(ifStep.thenStep)
    val newThenStep = ctx.optimizer
      .optimize(
        ifStep.thenStep :: Nil,
        Nil,
        ctx.env + (targetVar -> checkType),
        ctx.checkedVars,
      )
      ._1
      .toBlockStep

    val newElseStep = ifStep.elseStep.map(it =>
      ctx.optimizer
        .optimize(
          it :: Nil,
          Nil,
          ctx.env + (targetVar -> checkType),
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
      if (checkType == MayAbrupt && isAbruptTerminal)
        ctx.env + (targetVar -> MayCompletion)
      else ctx.env
    ctx.copy(ctx.input.tail, newIfStep :: ctx.history, newEnv)
  }
}

object LetStepCompletionRule extends OptimizeRule {
  import PolyfillInspector.*

  def apply(ctx: Context) = ctx.input match {
    case (step @ LetStep(Variable(name, _, _, _), _)) :: tail =>
      val (unwrappedLetStep, newEnv) =
        ctx.optimizer.transformStep(step, ctx.env, ctx.checkedVars)
      unwrappedLetStep match {
        case Some(newLetStep) =>
          if (newEnv(name) == MayAbrupt) {
            Some(
              ctx.copy(
                tail,
                getHoistedFlagSetting(
                  s"${name}_flag",
                  "abrupt",
                  newEnv,
                ) :: ValueAccessUnwrapper(ctx.env)
                  .walk(newLetStep) :: ctx.history,
                newEnv,
              ),
            )
          } else
            Some(
              ctx.copy(
                tail,
                ValueAccessUnwrapper(ctx.env).walk(newLetStep) :: ctx.history,
                newEnv,
              ),
            )
        case None => ???
      }
    case _ => None
  }
}

object ShorthandInliningRule extends OptimizeRule {
  import PolyfillInspector.*

  override def apply(ctx: Context): Option[Context] = ctx.input match {
    case InvokeShorthandStep(name, args) :: tail =>
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
          ctx.copy(
            tail,
            transformedStep.getOrElse(
              raise(s"Cannot inline shorthand: ${name} : ${targetStep}"),
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

  override def apply(ctx: Context): Option[Context] = ctx.input match {
    // XRefExpressionOperator.Algo = Let x be the algorithm steps defined in...
    case (step @ LetStep(
          Variable(name, _, _, _),
          XRefExpression(XRefExpressionOperator.Algo, id),
        )) :: tail =>
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
            TypeEnv(),
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
          ctx.copy(
            tail,
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
          case Abrupt | Throw => Some((MayAbrupt, extractVarName(expr)))
          case Normal         => Some((MayNormal, extractVarName(expr)))
          case _              => None
        }
      case CompoundCondition(left, op, right) =>
        traverseCondition(left).orElse(traverseCondition(right))
      case _ => None
    }

  private def extractVarName(expr: Expression) = expr match {
    case ReferenceExpression(Variable(x, _, _, _)) => x
    case err =>
      raise(
        s"Expected Reference Expression for extractVarName, but got '${err.toString}'",
      )
  }
}

// =============================================================================
// Value Access Unwrapper
// =============================================================================

private class ValueAccessUnwrapper(env: TypeEnv) extends LangWalker {

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
      env(varName) match {
        case NotCompletion => super.walk(expr)
        case _ =>
          ReferenceExpression(Variable(varName, Some("value_unwrapped")))
      }
    // Unwrap Completion AO calls
    case completionAO @ InvokeAbstractOperationExpression(name, args, _)
        if name.contains("Completion") =>
      if (args.length > 1)
        raise(
          s"Completion AO Call should contain up to one argument:\n\t$completionAO",
        )
      args.head
    // AO calls with completion argument unpacking
    case aoExpr @ InvokeAbstractOperationExpression(name, args, _) =>
      val newArgs = args.flatMap {
        case x @ ReferenceExpression(v @ Variable(targetVar, nt, _, _))
            if nt.isEmpty =>
          env(targetVar) match {
            case MayAbrupt | MayNormal | MayCompletion =>
              // Plug in x_flag directly — no numeric conversion
              List(
                ReferenceExpression(Variable(s"${targetVar}_flag", None)),
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
            raise(
              s"Completion AO Call should contain up to one argument:\n\t$c",
            )
          innerCallName match {
            case "NormalCompletion" =>
              List(EnumLiteral("normal"), innerArgs.head)
            case "ThrowCompletion" | "AbruptCompletion" =>
              List(EnumLiteral("abrupt"), innerArgs.head)
            case "Completion" =>
              raise(
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
