package esmeta.es.util
import esmeta.lang.*
import esmeta.lang.util.Parser.step

import scala.annotation.tailrec

private type Completion = "normal" | "abrupt"

sealed trait CompletionType
case object NormalCompletion extends CompletionType
case object AbruptCompletion extends CompletionType
case object UnknownCompletion extends CompletionType

object PolyfillInspector {

  def process(step: Step): Step = {
    val transformedStep = transform(step)
    optimizeCompletion(transformedStep :: Nil, Nil, Map()).toBlockStep
  }

  private def transform(step: Step): Step = step match {
    case BlockStep(StepBlock(steps)) =>
      val optimizedSubSteps = steps.map { sub =>
        sub.copy(step = transform(sub.step))
      }
      val rawStmts = optimizedSubSteps.map(_.step)
      val mergedStmts = optimize(rawStmts)
      BlockStep(StepBlock(mergedStmts.map(SubStep(None, _))))
    case IfStep(cond, thenStep, elseStep, config) =>
      IfStep(cond, transform(thenStep), elseStep.map(transform), config)
    case other => other
  }

  private def optimize(stmts: List[Step]): List[Step] = {
    handleCompletionCheck(stmts, Nil, Set())
  }

  @tailrec
  private def optimizeCompletion(
    input: List[Step],
    history: List[Step],
    env: Map[String, CompletionType],
  ): List[Step] = input match {
    case head :: tail =>

      val (newStep, newEnv) = transformStep(head, env)
      newStep match {
        case Some(x) =>
          val unwrappedStep = StepMapper.mapExpressions(x) {expr => unwrapValueAccess(expr, env)}
          optimizeCompletion(tail, unwrappedStep :: history, newEnv)
        case None    => optimizeCompletion(tail, history, newEnv)
      }
    case Nil => history.reverse
  }

  private def optimizeExpr(
    expr: Expression,
    env: Map[String, CompletionType],
  ): (Expression, Option[CompletionType]) = expr match {
    case InvokeAbstractOperationExpression(
          "Completion",
          args,
          _,
        ) => // Remove Completion
      (args.head, Some(UnknownCompletion))
    case ReferenceExpression(Variable(name, _)) => // Type propagation
      (expr, env.get(name))
    case _ => (expr, None)
  }

  private def transformStep(
    step: Step,
    env: Map[String, CompletionType],
  ): (Option[Step], Map[String, CompletionType]) = step match {

    case LetStep(v @ Variable(name, _), expr) =>
      val (newExpr, typeUpdate) = optimizeExpr(expr, env)
      val newEnv = typeUpdate.map(t => env + (name -> t)).getOrElse(env)
      (Some(LetStep(v, newExpr)), newEnv)

    case SetStep(v @ Variable(name, _), expr) =>
      expr match {
        case ReturnIfAbruptExpression( // Remove ! (shorthand)
              ReferenceExpression(Variable(inner, _)),
              false,
            ) if name == inner =>
          (None, env)
        case _ =>
          val (newExpr, typeUpdate) = optimizeExpr(expr, env)
          val newEnv = typeUpdate.map(t => env + (name -> t)).getOrElse(env)
          (Some(SetStep(v, newExpr)), newEnv)
      }

    case WrappedTryCatchStep(
          tryStep,
          catchVarRef @ Variable(catchVar, _),
          catchStep,
        ) =>
      val newTry = optimizeCompletion(tryStep :: Nil, Nil, env).toBlockStep

      val catchEnv = env + (catchVar -> AbruptCompletion)
      val newCatch = catchStep.map(c =>
        optimizeCompletion(c :: Nil, Nil, catchEnv).toBlockStep,
      )

      (
        Some(WrappedTryCatchStep(newTry, catchVarRef, newCatch)),
        env,
      )

    case check @ CompletionCheckPattern(checks) =>
      val (checkType, targetVar) = checks
      val ifStep = check.asInstanceOf[IfStep]
      val bodyStmt = optimizeCompletion(
        ifStep.thenStep :: Nil,
        Nil,
        env + (targetVar -> (if (checkType == "normal") NormalCompletion
                             else AbruptCompletion)),
      ).toBlockStep
      val elseStmt = ifStep.elseStep.map(step =>
        optimizeCompletion(
          step :: Nil,
          Nil,
          env + (targetVar -> (if (checkType == "abrupt") NormalCompletion
                               else AbruptCompletion)),
        ).toBlockStep,
      )

      (
        Some(
          ifStep.copy(
            thenStep = bodyStmt,
            elseStep = elseStmt,
          ),
        ),
        env,
      )

    case ret @ ReturnStep(
          ReturnIfAbruptExpression(ReferenceExpression(Variable(name, _)), true),
        ) =>
      env.get(name) match {
        case Some(AbruptCompletion) =>
          (Some(TaggedStep(ThrowStep(name), Map("reason" -> "abrupt"))), env)
        case _ => (Some(ret), env)
      }

    case TaggedStep(taggedInnerStep, tag) => taggedInnerStep match {
      case IfStep(c, t, e, _) =>
        transformStep(taggedInnerStep, env + (tag.get("TYPE") match {
          case Some("abrupt") => (tag.getOrElse("USE_FLAG", "") -> AbruptCompletion)
          case Some("normal") => (tag.getOrElse("USE_FLAG", "") -> NormalCompletion)
          case _ => ???
        })) match {
          case (None, env) => (None, env)
          case (Some(it), env) => (Some(TaggedStep(it, tag)), env)
        }

      case _ => transformStep(taggedInnerStep, env)
    }

    case BlockStep(stmts) =>
      (
        Some(optimizeCompletion(stmts.steps.map(_.step), Nil, env).toBlockStep),
        env,
      )

    case IfStep(cond, t, e, cfg) =>
      val newT = optimizeCompletion(t :: Nil, Nil, env).toBlockStep
      val newE = e.map(b => optimizeCompletion(b :: Nil, Nil, env).toBlockStep)
      (Some(IfStep(cond, newT, newE, cfg)), env)

    case _ => (Some(step), env)
  }

  private def unwrapValueAccess(
    expr: Expression,
    env: Map[String, CompletionType],
  ): Expression = expr match {
    case ReferenceExpression(Access(Variable(varName, _), "Value", _, _)) => // completion.[[Value]]
      env.get(varName) match {
        case Some(_) => ReferenceExpression(Variable(varName))
        case None => expr
      }
    case _ => expr
  }

  @tailrec
  private def handleCompletionCheck(
    input: List[Step],
    history: List[Step],
    handledVars: Set[String],
  ): List[Step] = input match {
    case (check @ CompletionCheckPattern(checks)) :: tail =>
      val ifStep = check.asInstanceOf[IfStep] // Always possible
      val (checkType, targetVar) = checks

      if (handledVars.contains(targetVar)) {
        val flagName = s"${targetVar}_is_abrupt"
        val taggedCheck = annotateStep(annotateStep(check, "USE_FLAG", flagName), "TYPE", checkType)

        println(s"$checks : $check (reassigned: $checkType)")
        handleCompletionCheck(tail, taggedCheck :: history, handledVars)
      } else {
        val lastModifiedAt =
          history.indexWhere(stmt => modifies(stmt, targetVar))
        if (lastModifiedAt != -1) {
          val (gap, rest) = history.splitAt(lastModifiedAt)
          val (producer, newHistory) = rest.splitAt(1)
          val producerBlock = producer.reverse

          val flagName = s"${targetVar}_is_abrupt"
          val flagDecl = LetStep(Variable(flagName, None), FalseLiteral())
          println(s"$checks : $producerBlock (gap: ${gap.length})")

          val isAbruptTerminal =
            checkType == "abrupt" && isTerminal(ifStep.thenStep)
          if (gap.isEmpty) {
            // No gap between checking; immediately wrap with try/catch
            val merged =
              mergeWithFlag(
                producer,
                check,
                targetVar,
                s"_${targetVar}_err",
                flagName,
                checkType,
                isAbruptTerminal,
              )
            handleCompletionCheck(
              tail,
              if (isAbruptTerminal) merged :: newHistory
              else merged :: flagDecl :: newHistory,
              handledVars + targetVar,
            )
          } else {
            // There are some gap between checking; just set the flag
            val wrappedProducer =
              wrapProducerOnly(
                producer,
                targetVar,
                s"_${targetVar}_err",
                flagName,
                isAbruptTerminal,
              )
            val taggedCheck = annotateStep(check, "USE_FLAG", flagName)
            handleCompletionCheck(
              tail,
              flagDecl :: wrappedProducer :: gap ::: taggedCheck :: newHistory,
              handledVars + targetVar,
            )
          }
        } else {
          // This variable must be parameter
          handleCompletionCheck(tail, check :: history, handledVars)
        }
      }
    case head :: tail =>
      handleCompletionCheck(tail, head :: history, handledVars)
    case Nil => history.reverse
  }

  private def modifies(stmt: Step, varName: String): Boolean = stmt match {
    case LetStep(n, _) => n.name == varName
    // TODO Assume only Variable Expression
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
    completion: String,
    isAbruptTerminal: Boolean,
  ): WrappedTryCatchStep = {
    val IfStep(_, bodyStep, _, _) =
      ifStep: @unchecked // Since CompletionCheckPattern only captures IfStep, this will always success
    if (completion == "normal") {
      val tryStmts = producer :+ bodyStep
      val catchStmts = List(
        SetStep(
          Variable(varName, None),
          ReferenceExpression(Variable(catchVar, None)),
        ),
        SetStep(Variable(flagName, None), TrueLiteral()),
      )
      WrappedTryCatchStep(
        tryStmts.toBlockStep,
        Variable(catchVar),
        Some(catchStmts.toBlockStep),
      )
    } else {
      val tryStmts = producer
      val catchStmts =
        if (!isAbruptTerminal)
          List(
            SetStep(
              Variable(varName, None),
              ReferenceExpression(Variable(catchVar, None)),
            ),
            SetStep(Variable(flagName, None), TrueLiteral()),
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
      WrappedTryCatchStep(
        tryStmts.toBlockStep,
        Variable(catchVar),
        Some(catchStmts.toBlockStep),
      )
    }
  }

  private def wrapProducerOnly(
    producer: List[Step],
    varName: String,
    catchVar: String,
    flagName: String,
    isAbruptTerminal: Boolean,
  ): Step = {
    val tryStmts = producer
    val catchStmts = List(
      SetStep(
        Variable(varName, None),
        ReferenceExpression(Variable(catchVar, None)),
      ),
      SetStep(Variable(flagName, None), TrueLiteral()),
    )
    WrappedTryCatchStep(
      tryStmts.toBlockStep,
      Variable(catchVar),
      Some(catchStmts.toBlockStep),
    )
  }

  extension (l: List[Step])
    def toBlockStep = BlockStep(StepBlock(l.map(SubStep(None, _))))

}

private object CompletionCheckPattern {
  // TODO: Considering cases that has up to 1 completion checker
  def unapply(step: Step): Option[(Completion, String)] = step match {
    case IfStep(cond, thenStep, elseStep, config) => traverseCondition(cond)
    case _                                        => None
  }

  private def traverseCondition(cond: Condition): Option[(Completion, String)] =
    cond match {
      case PredicateCondition(expr, _, op) =>
        import PredicateConditionOperator.*
        op match {
          case Abrupt | Throw => Some(("abrupt", extractVarName(expr)))
          case Normal         => Some(("normal", extractVarName(expr)))
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

object StepMapper {
  def mapExpressions(step: Step)(f: Expression => Expression): Step =
    step match {
      case LetStep(v, expr) =>
        LetStep(v, f(expr))
      case SetStep(ref, expr) =>
        SetStep(mapRef(ref)(f), f(expr))
      case SetAsStep(ref, verb, id) =>
        SetAsStep(mapRef(ref)(f), verb, id)
      case SetEvaluationStateStep(context, func, args) =>
        SetEvaluationStateStep(mapRef(context)(f), func, args.map(f))
      case PerformStep(expr) =>
        PerformStep(f(expr))
      case InvokeShorthandStep(name, args) =>
        InvokeShorthandStep(name, args.map(f))
      case AppendStep(elem, ref) =>
        AppendStep(f(elem), mapRef(ref)(f))
      case PrependStep(elem, ref) =>
        PrependStep(f(elem), mapRef(ref)(f))
      case InsertStep(elem, ref) =>
        InsertStep(f(elem), mapRef(ref)(f))
      case AddStep(elem, ref) =>
        AddStep(f(elem), mapRef(ref)(f))
      case RemoveStep(target, prep, list) =>
        RemoveStep(mapRemoveTarget(target)(f), prep, f(list))
      case PushContextStep(ref) =>
        PushContextStep(mapRef(ref)(f))
      case SuspendStep(variable, remove) =>
        SuspendStep(variable, remove)
      case RemoveContextStep(context, restoreTarget) =>
        RemoveContextStep(
          mapRef(context)(f),
          mapRestoreTarget(restoreTarget)(f),
        )
      case AssertStep(cond) =>
        AssertStep(mapCond(cond)(f))
      case IfStep(cond, thenStep, elseStep, config) =>
        IfStep(
          mapCond(cond)(f),
          mapExpressions(thenStep)(f),
          elseStep.map(mapExpressions(_)(f)),
          config,
        )
      case RepeatStep(cond, body) =>
        RepeatStep(mapLoopCond(cond)(f), mapExpressions(body)(f))
      case ForEachStep(ty, variable, expr, forward, body) =>
        ForEachStep(ty, variable, f(expr), forward, mapExpressions(body)(f))
      case ForEachIntegerStep(
            variable,
            low,
            lowInc,
            high,
            highInc,
            ascending,
            body,
          ) =>
        ForEachIntegerStep(
          variable,
          f(low),
          lowInc,
          f(high),
          highInc,
          ascending,
          mapExpressions(body)(f),
        )
      case ForEachOwnPropertyKeyStep(key, obj, cond, ascending, order, body) =>
        ForEachOwnPropertyKeyStep(
          key,
          obj,
          mapCond(cond)(f),
          ascending,
          order,
          mapExpressions(body)(f),
        )
      case ForEachParseNodeStep(variable, expr, body) =>
        ForEachParseNodeStep(variable, f(expr), mapExpressions(body)(f))
      case ReturnStep(expr) =>
        ReturnStep(f(expr))
      case ThrowStep(name) =>
        ThrowStep(name)
      case ResumeStep(
            callerContext,
            argument,
            generatorContext,
            param,
            steps,
          ) =>
        ResumeStep(
          mapRef(callerContext)(f),
          f(argument),
          mapRef(generatorContext)(f),
          param,
          steps.map(mapSubStep(_)(f)),
        )
      case ResumeEvaluationStep(context, argument, param, steps) =>
        ResumeEvaluationStep(
          mapRef(context)(f),
          argument.map(f),
          param,
          steps.map(mapSubStep(_)(f)),
        )
      case ResumeTopContextStep() =>
        ResumeTopContextStep()
      case NoteStep(note) =>
        NoteStep(note)
      case BlockStep(StepBlock(steps)) =>
        BlockStep(StepBlock(steps.map(mapSubStep(_)(f))))
      case YetStep(expr) =>
        YetStep(expr)
      case SetFieldsWithIntrinsicsStep(ref, desc) =>
        SetFieldsWithIntrinsicsStep(mapRef(ref)(f), desc)
      case PerformBlockStep(StepBlock(steps), desc) =>
        PerformBlockStep(StepBlock(steps.map(mapSubStep(_)(f))), desc)
      case WrappedTryCatchStep(tryBlock, catchVar, catchBlock) =>
        WrappedTryCatchStep(
          mapExpressions(tryBlock)(f),
          catchVar,
          catchBlock.map(mapExpressions(_)(f)),
        )
      case TaggedStep(innerStep, tag) =>
        TaggedStep(mapExpressions(innerStep)(f), tag)
    }

  private def mapSubStep(sub: SubStep)(f: Expression => Expression): SubStep =
    sub.copy(step = mapExpressions(sub.step)(f))

  private def mapRef(ref: Reference)(f: Expression => Expression): Reference =
    ref match {
      case v: Variable => v
      case Access(base, name, kind, form) =>
        Access(mapRef(base)(f), name, kind, form)
      case ValueOf(base)              => ValueOf(mapRef(base)(f))
      case IntrinsicField(base, intr) => IntrinsicField(mapRef(base)(f), intr)
      case IndexLookup(base, index)   => IndexLookup(mapRef(base)(f), f(index))
      case BindingLookup(base, binding) =>
        BindingLookup(mapRef(base)(f), f(binding))
      case NonterminalLookup(base, nt) => NonterminalLookup(mapRef(base)(f), nt)
      case PositionalElement(base, isFirst) =>
        PositionalElement(mapRef(base)(f), isFirst)
      case IntrinsicObject(base, expr) =>
        IntrinsicObject(mapRef(base)(f), f(expr))
      case r: RunningExecutionContext => r
      case r: SecondExecutionContext  => r
      case r: CurrentRealmRecord      => r
      case r: ActiveFunctionObject    => r
      case r: AgentRecord             => r
    }

  private def mapCond(cond: Condition)(f: Expression => Expression): Condition =
    cond match {
      case ExpressionCondition(expr) => ExpressionCondition(f(expr))
      case TypeCheckCondition(expr, neg, tys) =>
        TypeCheckCondition(f(expr), neg, tys)
      case HasFieldCondition(ref, neg, field, form) =>
        HasFieldCondition(mapRef(ref)(f), neg, field, form)
      case HasBindingCondition(ref, neg, binding) =>
        HasBindingCondition(mapRef(ref)(f), neg, f(binding))
      case ProductionCondition(nt, lhsName, rhsName) =>
        ProductionCondition(nt, lhsName, rhsName)
      case PredicateCondition(expr, neg, op) =>
        PredicateCondition(f(expr), neg, op)
      case IsAreCondition(left, neg, right) =>
        IsAreCondition(left.map(f), neg, right.map(f))
      case BinaryCondition(left, op, right) =>
        BinaryCondition(f(left), op, f(right))
      case InclusiveIntervalCondition(left, neg, from, to, desc) =>
        InclusiveIntervalCondition(f(left), neg, f(from), f(to), desc)
      case ContainsCondition(list, neg, target) =>
        ContainsCondition(f(list), neg, mapContainsTarget(target)(f))
      case CompoundCondition(left, op, right) =>
        CompoundCondition(mapCond(left)(f), op, mapCond(right)(f))
    }

  private def mapContainsTarget(
    target: ContainsConditionTarget,
  )(f: Expression => Expression): ContainsConditionTarget =
    target match {
      case ContainsConditionTarget.Expr(expr) =>
        ContainsConditionTarget.Expr(f(expr))
      case other => other
    }

  private def mapRemoveTarget(
    target: RemoveStep.Target,
  )(f: Expression => Expression): RemoveStep.Target =
    target match {
      case RemoveStep.Target.First(count) =>
        RemoveStep.Target.First(count.map(f))
      case RemoveStep.Target.Last(count) => RemoveStep.Target.Last(count.map(f))
      case RemoveStep.Target.Element(elem) => RemoveStep.Target.Element(f(elem))
    }

  private def mapRestoreTarget(
    target: RemoveContextStep.RestoreTarget,
  )(f: Expression => Expression): RemoveContextStep.RestoreTarget =
    target match {
      case RemoveContextStep.RestoreTarget.NoRestore =>
        RemoveContextStep.RestoreTarget.NoRestore
      case RemoveContextStep.RestoreTarget.StackTop =>
        RemoveContextStep.RestoreTarget.StackTop
      case RemoveContextStep.RestoreTarget.Context(ref) =>
        RemoveContextStep.RestoreTarget.Context(mapRef(ref)(f))
    }

  private def mapLoopCond(
    cond: RepeatStep.LoopCondition,
  )(f: Expression => Expression): RepeatStep.LoopCondition =
    cond match {
      case RepeatStep.LoopCondition.NoCondition =>
        RepeatStep.LoopCondition.NoCondition
      case RepeatStep.LoopCondition.While(c) =>
        RepeatStep.LoopCondition.While(mapCond(c)(f))
      case RepeatStep.LoopCondition.Until(c) =>
        RepeatStep.LoopCondition.Until(mapCond(c)(f))
    }
}
