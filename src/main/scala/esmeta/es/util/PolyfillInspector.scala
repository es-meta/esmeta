package esmeta.es.util
import esmeta.lang.*
import esmeta.lang.BinaryConditionOperator.Eq
import esmeta.lang.PredicateConditionOperator.Abrupt
import esmeta.lang.util.{Walker => LangWalker}
import esmeta.spec.*
import esmeta.ty.{NumberTy, ValueTy}

import scala.annotation.tailrec

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

// Combined environment for tracking completion types and handled variables
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

object PolyfillInspector {

  def process(algo: Algorithm): Algorithm = {
    val newHead = algo.head match {
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
    algo.copy(head = newHead)
  }

  def process(algo: Algorithm, step: Step): Step = {
    val paramCompletion = algo.head match {
      case ao @ AbstractOperationHead(_, _, params, _) =>
        params.filter {
          case p @ Param(name, Type(ty), paramKind) => ty.isCompletion
        }
      case x => List()
    }
    val env = paramCompletion.foldLeft(CompletionEnv())((it, item) =>
      it.withType(item.name, ParameterCompletion),
    )
    optimize(step :: Nil, Nil, env)._1.toBlockStep
  }

  private def optimize(
    input: List[Step],
    history: List[Step],
    env: CompletionEnv,
  ): (List[Step], CompletionEnv) = input match {
    case InvokeShorthandStep(name, args) :: tail if name.contains("IfAbrupt") =>
      // TODO Expect completion record to be first place
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
          ReferenceExpression(
            Variable(
              s"${targetVar}_is_abrupt",
              None,
            ),
          ),
          Eq,
          TrueLiteral(),
        ),
        transformStep(AbruptCompletion),
        Some(transformStep(NormalCompletion)),
      )

      if (!env.isHandled(targetVar)) {
        val lastModifiedAt =
          history.indexWhere(stmt => modifies(stmt, targetVar))
        if (lastModifiedAt != -1) {
          val (gap, rest) = history.splitAt(lastModifiedAt)
          val (producer, newHistory) = rest.splitAt(1)
          // There should be always gap between (possible) if conditions
          val flagName = s"${targetVar}_is_abrupt"
          val wrappedProducer = wrapProducerOnly(
            producer,
            targetVar,
            s"_${targetVar}_err",
            flagName,
            env,
          )
          val newEnv = env
            .withHandled(targetVar)
            .withType(targetVar, UnknownCompletion)
            .dropType(targetVar)
            .withFlag(flagName)

          optimize(
            tail,
            checkCondition :: wrappedProducer :: gap ::: newHistory,
            newEnv,
          )
        } else {
          // Parameter, simply split
          val transformedStep = InvokeShorthandStep(
            name,
            List(
              ReferenceExpression(Variable(s"${targetVar}_type")),
              ReferenceExpression(Variable(s"$targetVar")),
            ) ++ args.drop(1),
          )
          optimize(tail, transformedStep :: history, env.dropType(targetVar))
        }
      } else {
        optimize(tail, checkCondition :: history, env.dropType(targetVar))
      }

    case (check @ CompletionCheckPattern(checks)) :: tail =>
      val ifStep = check.asInstanceOf[IfStep]
      val (checkType, targetVar) = checks

      if (env.isHandled(targetVar))
        handleAlreadyHandled(
          tail,
          history,
          env,
          check,
          ifStep,
          checkType,
          targetVar,
        )
      else
        handleUnhandled(
          tail,
          history,
          env,
          check,
          ifStep,
          checkType,
          targetVar,
        )
    case (step @ LetStep(v @ Variable(name, _), expr)) :: tail =>
      val (unwrappedLetStep, newEnv) = transformStep(step, env)
      unwrappedLetStep match {
        case Some(newLetStep) =>
          if (newEnv.getType(name).contains(AbruptCompletion)) {
            optimize(
              tail,
              getHoistedFlagSetting(
                s"${name}_is_abrupt",
                true,
                newEnv,
              ) :: ValueAccessUnwrapper(env).walk(newLetStep) :: history,
              newEnv.withFlag(s"${name}_is_abrupt"),
            )
          } else
            optimize(
              tail,
              ValueAccessUnwrapper(env).walk(newLetStep) :: history,
              newEnv,
            )
        case None =>
          ???
          optimize(tail, history, newEnv)
      }

    case head :: tail =>
      val (newStepOpt, newEnv) = transformStep(head, env)
      newStepOpt match {
        case Some(newStep) =>
          val unwrapped = ValueAccessUnwrapper(env).walk(newStep)
          optimize(tail, unwrapped :: history, newEnv)
        case None =>
          optimize(tail, history, newEnv)
      }

    case Nil => (history.reverse, env)
  }

  /** Handle a completion check where the variable is already handled (wrapped
    * in try/catch). Either omit the check entirely (if normal in try body with
    * no else), or rewrite it to use a flag.
    */
  private def handleAlreadyHandled(
    tail: List[Step],
    history: List[Step],
    env: CompletionEnv,
    check: Step,
    ifStep: IfStep,
    checkType: CompletionType,
    targetVar: String,
  ): (List[Step], CompletionEnv) = {
    val canOmit = ifStep.elseStep.isEmpty &&
      env.getType(targetVar).contains(NormalCompletion)

    if (canOmit) {
      // Already surrounded by try (not catch), IfStep can be omitted
      transformStep(
        ifStep.thenStep,
        env.withType(targetVar, checkType),
      ) match {
        case (Some(optimizedThen), newEnv) =>
          optimize(tail, optimizedThen :: history, newEnv)
        case (None, newEnv) => optimize(tail, history, newEnv)
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
      transformStep(
        taggedCheck,
        env.withType(targetVar, checkType).withHandled(targetVar),
      ) match {
        case (Some(optimizedCheck), newEnv) =>
          optimize(tail, optimizedCheck :: history, newEnv)
        case (None, newEnv) => optimize(tail, history, newEnv)
      }
    }
  }

  /** Handle a completion check where the variable is not yet handled. Find the
    * producer in history and decide on a wrapping strategy.
    */
  private def handleUnhandled(
    tail: List[Step],
    history: List[Step],
    env: CompletionEnv,
    check: Step,
    ifStep: IfStep,
    checkType: CompletionType,
    targetVar: String,
  ): (List[Step], CompletionEnv) = {
    val lastModifiedAt =
      history.indexWhere(stmt => modifies(stmt, targetVar))

    if (lastModifiedAt != -1) {
      val (gap, rest) = history.splitAt(lastModifiedAt)
      val (producer, newHistory) = rest.splitAt(1)
      if (gap.isEmpty)
        handleNoGap(
          tail,
          newHistory,
          env,
          check,
          ifStep,
          checkType,
          targetVar,
          producer,
        )
      else
        handleWithGap(
          tail,
          gap,
          newHistory,
          env,
          check,
          ifStep,
          checkType,
          targetVar,
          producer,
        )
    } else {
      handleParameter(tail, history, env, ifStep, checkType, targetVar)
    }
  }

  /** Producer immediately precedes the check merge into a single try/catch.
    */
  private def handleNoGap(
    tail: List[Step],
    newHistory: List[Step],
    env: CompletionEnv,
    check: Step,
    ifStep: IfStep,
    checkType: CompletionType,
    targetVar: String,
    producer: List[Step],
  ): (List[Step], CompletionEnv) = {
    val flagName = s"${targetVar}_is_abrupt"
    val isAbruptTerminal =
      checkType == AbruptCompletion && isTerminal(ifStep.thenStep)

    val merged = mergeWithFlag(
      producer,
      check,
      targetVar,
      s"_${targetVar}_err",
      flagName,
      checkType,
      isAbruptTerminal,
      env,
    )
    transformStep(
      merged,
      env
        .withHandled(targetVar)
        .withType(targetVar, UnknownCompletion)
        .withFlag(flagName),
    ) match {
      case (Some(optimizedTryCatch), newEnv) =>
        optimize(
          tail,
          optimizedTryCatch :: newHistory,
          newEnv,
        )
      case (None, newEnv) => optimize(tail, newHistory, newEnv)
    }
  }

  /** Steps exist between producer and check wrap producer separately, handle
    * check with a flag.
    */
  private def handleWithGap(
    tail: List[Step],
    gap: List[Step],
    newHistory: List[Step],
    env: CompletionEnv,
    check: Step,
    ifStep: IfStep,
    checkType: CompletionType,
    targetVar: String,
    producer: List[Step],
  ): (List[Step], CompletionEnv) = {
    val flagName = s"${targetVar}_is_abrupt"

    val wrappedProducer = wrapProducerOnly(
      producer,
      targetVar,
      s"_${targetVar}_err",
      flagName,
      env,
    )
    val taggedCheck = annotateStep(
      annotateStep(
        annotateStep(check, "USE_FLAG", flagName),
        "TYPE",
        checkType.toTag,
      ),
      "TARGET_VAR",
      targetVar,
    )
    val newEnv = env
      .withHandled(targetVar)
      .withType(targetVar, UnknownCompletion)
      .withFlag(flagName)
    transformStep(
      taggedCheck,
      env
        .withType(targetVar, checkType)
        .withFlag(flagName),
    ) match {
      case (Some(optimizedCheck), newEnv) =>
        optimize(
          tail,
          optimizedCheck :: wrappedProducer :: gap ::: newHistory,
          newEnv,
        )
      case (None, newEnv) => ???
    }
  }

  /** At this block, the completion in condition must be in a function
    * signature(parameter). Then the completion should be unpacked as:
    * ${name}_type, ${name}
    *   - ${name}_type (Number) : 0 if normal, 1 if abrupt
    *   - ${name} (ECMAScript Value): unpacked completion value itself Thus, we
    *     have to make new IfStep to handle this logic.
    */
  private def handleParameter(
    tail: List[Step],
    history: List[Step],
    env: CompletionEnv,
    ifStep: IfStep,
    checkType: CompletionType,
    targetVar: String,
  ): (List[Step], CompletionEnv) = {
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
    val newThenStep = optimize(
      ifStep.thenStep :: Nil,
      Nil,
      env.withType(targetVar, checkType),
    )._1.toBlockStep

    val newElseStep = ifStep.elseStep.map(it =>
      optimize(
        it :: Nil,
        Nil,
        env.withType(targetVar, checkType),
      )._1.toBlockStep,
    )

    val newIfStep = ifStep.copy(
      cond = newCond,
      thenStep = newThenStep,
      elseStep = newElseStep,
    )
    val newEnv =
      if (checkType == AbruptCompletion && isAbruptTerminal)
        env.withType(targetVar, ResolvedParameterCompletion)
      else env
    optimize(tail, newIfStep :: history, newEnv)
  }

  @tailrec
  private def transformStep(
    step: Step,
    env: CompletionEnv,
  ): (Option[Step], CompletionEnv) = step match {

    case LetStep(v @ Variable(name, _), expr) =>
      val (newExpr, typeUpdate) = optimizeExpr(expr, env)
      if (!env.getType(name).contains(NormalCompletion))
        (
          Some(LetStep(v, newExpr)),
          typeUpdate.map(t => env.withType(name, t)).getOrElse(env),
        )
      else (Some(LetStep(v, newExpr)), env)
    case SetStep(v @ Variable(name, _), expr) =>
      expr match {
        case ReturnIfAbruptExpression(
              ReferenceExpression(Variable(inner, _)),
              false,
            ) if name == inner =>
          // Remove redundant Set x = ! x shorthand
          (None, env)
        case _ =>
          val (newExpr, typeUpdate) = optimizeExpr(expr, env)
          env.getType(name) match {
            case _ if typeUpdate.contains(UnknownCompletion) =>
              (
                Some(SetStep(v, newExpr)),
                typeUpdate
                  .map(t => env.withType(name, t))
                  .getOrElse(env)
                  .dropHandled(name),
              )
            case Some(NormalCompletion) => (Some(SetStep(v, newExpr)), env)
            case Some(_) =>
              (
                Some(SetStep(v, newExpr)),
                typeUpdate.map(t => env.withType(name, t)).getOrElse(env),
              )
            case None => (Some(SetStep(v, newExpr)), env)
          }
      }

    /*
    case WrappedTryCatchStep(
          tryStep,
          catchVarRef @ Variable(catchVar, _),
          catchStep,
        ) =>
      val newTry = tryStep
      val catchEnv = env.withType(catchVar, AbruptCompletion)
      val newCatch =
        catchStep.map(c => optimize(c :: Nil, Nil, catchEnv).toBlockStep)
      (Some(WrappedTryCatchStep(newTry, catchVarRef, newCatch)), env)
     */
    case ret @ ReturnStep(
          ReturnIfAbruptExpression(ReferenceExpression(Variable(name, _)), true),
        ) =>
      env.getType(name) match {
        case Some(AbruptCompletion) =>
          (Some(TaggedStep(ThrowStep(name), Map("reason" -> "abrupt"))), env)
        case Some(ParameterCompletion) =>
          (
            Some(
              IfStep(
                BinaryCondition(
                  ReferenceExpression(
                    Variable(
                      s"${name}_type",
                      None,
                    ),
                  ),
                  Eq,
                  NumberLiteral(1),
                ),
                TaggedStep(ThrowStep(name), Map("reason" -> "abrupt")),
                Some(ret),
              ),
            ),
            env.withType(name, ResolvedParameterCompletion),
          )
        case Some(UnknownCompletion) =>
          (
            Some(
              IfStep(
                BinaryCondition(
                  ReferenceExpression(
                    Variable(
                      s"${name}_is_abrupt",
                      None,
                    ),
                  ),
                  Eq,
                  TrueLiteral(),
                ),
                TaggedStep(ThrowStep(name), Map("reason" -> "abrupt")),
                Some(ret),
              ),
            ),
            env.withType(name, ResolvedParameterCompletion),
          )
        case _ => (Some(ret), env)
      }
    case ret @ ReturnStep(expr) =>
      expr match {
        case InvokeAbstractOperationExpression(
              name,
              ReferenceExpression(Variable(varName, nt)) :: Nil,
              _,
            ) if (name == "ThrowCompletion") =>
          (Some(TaggedStep(ThrowStep(varName), Map("reason" -> "abrupt"))), env)
        case ReferenceExpression(Variable(name, _))
            if env.getType(name).contains(AbruptCompletion) =>
          (Some(TaggedStep(ThrowStep(name), Map("reason" -> "abrupt"))), env)
        case _ => (Some(ret), env)
      }

    case TaggedStep(taggedInnerStep, tag) =>
      taggedInnerStep match {
        case IfStep(cond, thenStep, elseStep, cfg) =>
          // Get the target variable and check type from tags
          val targetVarOpt = tag.get("TARGET_VAR")
          val checkTypeOpt = tag.get("TYPE").map(CompletionType.fromTag)

          (targetVarOpt, checkTypeOpt) match {
            case (Some(targetVar), Some(checkType)) =>
              // Process branches with correct completion type for target variable
              val thenType =
                if (checkType == AbruptCompletion) AbruptCompletion
                else NormalCompletion
              val elseType =
                if (checkType == AbruptCompletion) NormalCompletion
                else AbruptCompletion

              val thenEnv = env.withType(targetVar, thenType)
              val elseEnv = env.withType(targetVar, elseType)

              val newThen =
                optimize(thenStep :: Nil, Nil, thenEnv)._1.toBlockStep
              val newElse =
                elseStep.map(e =>
                  optimize(e :: Nil, Nil, elseEnv)._1.toBlockStep,
                )

              val flagVar = tag.getOrElse("USE_FLAG", s"${targetVar}_is_abrupt")
              rebaseCondition(
                cond,
                Map(
                  targetVar -> BinaryCondition(
                    ReferenceExpression(
                      Variable(flagVar, None),
                    ),
                    Eq,
                    if (checkType == AbruptCompletion) TrueLiteral()
                    else FalseLiteral(),
                  ),
                ),
              ) match {
                case Some(newCond) =>
                  (
                    Some(
                      TaggedStep(IfStep(newCond, newThen, newElse, cfg), tag),
                    ),
                    env,
                  )
                // TODO Can we ignore ElseStep? If not, how can we handle it?
                case None =>
                  (
                    Some(newThen),
                    env,
                  ) // If cond is empty, IfStep can be omitted
              }

            case _ =>
              // No completion check tags - just process normally
              val newThen = optimize(thenStep :: Nil, Nil, env)._1.toBlockStep
              val newElse =
                elseStep.map(e => optimize(e :: Nil, Nil, env)._1.toBlockStep)
              rebaseCondition(
                cond,
                Map(),
              ) match {
                case Some(newCond) =>
                  (
                    Some(
                      TaggedStep(IfStep(newCond, newThen, newElse, cfg), tag),
                    ),
                    env,
                  )
                // TODO Can we ignore ElseStep? If not, how can we handle it?
                case None =>
                  (
                    Some(newThen),
                    env,
                  ) // If cond is empty, IfStep can be omitted
              }
          }
        case _ => transformStep(taggedInnerStep, env)
      }

    case BlockStep(stmts) =>
      val (newSteps, newEnv) = optimize(stmts.steps.map(_.step), Nil, env)
      (Some(newSteps.toBlockStep), newEnv)

    case IfStep(cond, t, e, cfg) =>
      val (thenSteps, thenEnv) = optimize(t :: Nil, Nil, env)
      val (elseResult, elseEnv) = e match {
        case Some(b) =>
          val (steps, eEnv) = optimize(b :: Nil, Nil, env)
          (Some(steps.toBlockStep), eEnv)
        case None => (None, env)
      }
      val mergedEnv = thenEnv.merge(elseEnv)
      (Some(IfStep(cond, thenSteps.toBlockStep, elseResult, cfg)), mergedEnv)
    case RepeatStep(c, b) =>
      (Some(RepeatStep(c, optimize(b :: Nil, Nil, env)._1.toBlockStep)), env)
    case _ => (Some(step), env)
  }

  private def getHoistedFlagSetting(
    flagName: String,
    boolLiteral: Boolean,
    env: CompletionEnv,
  ): Step = {
    val boolLiteralExpr = if (boolLiteral) TrueLiteral() else FalseLiteral()
    if (!env.isFlagDeclared(flagName))
      LetStep(Variable(flagName, None), boolLiteralExpr)
    else SetStep(Variable(flagName, None), boolLiteralExpr)
  }

  private def optimizeExpr(
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

  private def modifies(stmt: Step, varName: String): Boolean = stmt match {
    case LetStep(n, _)           => n.name == varName
    case SetStep(x: Variable, _) => x.name == varName
    case IfStep(_, t, e, _) =>
      modifies(t, varName) || e.exists(modifies(_, varName))
    case BlockStep(StepBlock(stmts)) =>
      stmts.exists(it => modifies(it.step, varName))
    case _ => false
  }

  private def isTerminal(stmt: Step): Boolean = stmt match {
    case ReturnStep(_) => true
    case ThrowStep(_)  => true
    case BlockStep(StepBlock(steps)) =>
      steps.lastOption.exists(it => isTerminal(it.step))
    case IfStep(_, t, Some(e), _)           => isTerminal(t) && isTerminal(e)
    case WrappedTryCatchStep(t, _, Some(c)) => isTerminal(t) && isTerminal(c)
    case _                                  => false
  }

  private def annotateStep(
    step: Step,
    name: String,
    value: String,
  ): TaggedStep =
    step match {
      case TaggedStep(realStep, existingTag) =>
        TaggedStep(realStep, existingTag + (name -> value))
      case x => TaggedStep(x, Map(name -> value))
    }

  private def mergeWithFlag(
    producer: List[Step],
    ifStep: Step,
    varName: String,
    catchVar: String,
    flagName: String,
    completion: CompletionType,
    isAbruptTerminal: Boolean,
    env: CompletionEnv,
  ): WrappedTryCatchStep = {
    val IfStep(_, bodyStep, _, _) = ifStep: @unchecked
    if (completion == NormalCompletion) {
      val (tryStmts, _) = optimize(
        producer :+ bodyStep,
        Nil,
        env
          .withHandled(varName)
          .withType(varName, NormalCompletion)
          .withFlag(flagName),
      )
      val catchStmts = List(
        SetStep(
          Variable(varName, None),
          ReferenceExpression(Variable(catchVar, None)),
        ),
        getHoistedFlagSetting(flagName, true, env),
      )
      WrappedTryCatchStep(
        tryStmts.toBlockStep,
        Variable(catchVar),
        Some(catchStmts.toBlockStep),
      )
    } else {
      val (tryStmts, _) = optimize(
        producer,
        Nil,
        env
          .withHandled(varName)
          .withType(varName, NormalCompletion)
          .withFlag(flagName),
      )
      val catchStmts =
        if (!isAbruptTerminal)
          List(
            SetStep(
              Variable(varName, None),
              ReferenceExpression(Variable(catchVar, None)),
            ),
            getHoistedFlagSetting(flagName, true, env),
            bodyStep,
          )
        else
          List(
            SetStep(
              Variable(varName, None),
              ReferenceExpression(Variable(catchVar, None)),
            ),
            bodyStep,
          )
      val (optimizedCatchStmts, _) = optimize(
        catchStmts,
        Nil,
        env
          .withHandled(varName)
          .withType(varName, AbruptCompletion)
          .withFlag(flagName),
      )
      WrappedTryCatchStep(
        tryStmts.toBlockStep,
        Variable(catchVar),
        Some(optimizedCatchStmts.toBlockStep),
      )
    }
  }

  private def wrapProducerOnly(
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

  private def rebaseCondition(
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
      // TODO: Is it safe to ignore op?
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

  extension (l: List[Step])
    def toBlockStep: Step = l match {
      case (b: BlockStep) :: Nil => b
      case _ => BlockStep(StepBlock(l.map(SubStep(None, _))))
    }
}

private object CompletionCheckPattern {
  // TODO: Considering cases that has up to 1 completion checker
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

private class ValueAccessUnwrapper(env: CompletionEnv) extends LangWalker {

  // Separated walk operation to keep LangWalker intact
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
