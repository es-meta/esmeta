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

  private def optimizeCompletion(
    input: List[Step],
    history: List[Step],
    env: Map[String, CompletionType],
  ): List[Step] = input match {
    /*case (x @ LetStep(Variable(varName, _), expr)) :: tail =>
      expr match {
        case InvokeAbstractOperationExpression(aoName, args, _) =>
          if (aoName == "Completion")
            optimizeCompletion(
              tail,
              x.copy(expr =
                args.head,
              ) :: history, // AO_Completion only has 1 arg
              env + (varName -> UnknownCompletion),
            )
          else optimizeCompletion(tail, x :: history, env)
        case ReferenceExpression(Variable(name, _)) =>
          val newEnv = env.get(name) match {
            case Some(x) => env + (varName -> x)
            case None    => env
          }
          optimizeCompletion(tail, x :: history, newEnv)
        case _ => optimizeCompletion(tail, x :: history, env)
      }
    case (x @ SetStep(Variable(varName, _), expr)) :: tail =>
      expr match {
        case InvokeAbstractOperationExpression(aoName, args, _) =>
          if (aoName == "Completion")
            optimizeCompletion(
              tail,
              x.copy(expr =
                args.head,
              ) :: history, // AO_Completion only has 1 arg
              env + (varName -> UnknownCompletion),
            )
          else optimizeCompletion(tail, x :: history, env)
        case ReferenceExpression(Variable(name, _)) =>
          val newEnv = env.get(name) match {
            case Some(x) => env + (varName -> x)
            case None    => env
          }
          optimizeCompletion(tail, x :: history, newEnv)
        case ReturnIfAbruptExpression(
              ReferenceExpression(Variable(innerVarNm, _)),
              false,
            ) =>
          if (varName == innerVarNm)
            optimizeCompletion(
              tail,
              history,
              env,
            ) // Remove ! (unwrapping shorthand)
          else optimizeCompletion(tail, x :: history, env)
        case _ => optimizeCompletion(tail, x :: history, env)
      }
    case WrappedTryCatchStep(
          BlockStep(StepBlock(tryBlock)),
          catchVarRef @ Variable(catchVar, _),
          Some(BlockStep(StepBlock(catchBlock))),
        ) :: tail =>
      val tryStmt = optimizeCompletion(tryBlock.map(_.step), Nil, env)
      val catchStmt = optimizeCompletion(
        catchBlock.map(_.step),
        Nil,
        env + (catchVar -> AbruptCompletion),
      )
      optimizeCompletion(
        tail,
        WrappedTryCatchStep(
          tryStmt.toBlockStep,
          catchVarRef,
          Some(catchStmt.toBlockStep),
        ) :: history,
        env,
      )
    case (check @ CompletionCheckPattern(checks)) :: tail =>
      val (checkType, targetVar) = checks
      val ifStep = check.asInstanceOf[IfStep]
      val bodyStmt = optimizeCompletion(
        ifStep.thenStep :: Nil,
        Nil,
        env + (targetVar -> (if (checkType == "normal") NormalCompletion
                             else AbruptCompletion)),
      )
      ifStep.elseStep match {
        case Some(step) =>
          val elseStmt = optimizeCompletion(
            step :: Nil,
            Nil,
            env + (targetVar -> (if (checkType == "abrupt") NormalCompletion
                                 else AbruptCompletion)),
          )
          optimizeCompletion(
            tail,
            ifStep.copy(
              thenStep = bodyStmt.toBlockStep,
              elseStep = Some(elseStmt.toBlockStep),
            ) :: Nil,
            env,
          )
        case None =>
          optimizeCompletion(
            tail,
            ifStep.copy(thenStep = bodyStmt.toBlockStep) :: history,
            env,
          )
      }
    case BlockStep(StepBlock(stmts)) :: tail =>
      val innerStmt = optimizeCompletion(stmts.map(_.step), Nil, env)
      optimizeCompletion(tail, innerStmt.toBlockStep :: history, env)
    case (x @ ReturnStep(ReturnIfAbruptExpression(expr, true))) :: tail =>
      expr match {
        case ReferenceExpression(Variable(name, _)) =>
          env.get(name) match {
            case Some(AbruptCompletion) =>
              optimizeCompletion(
                tail,
                TaggedStep(
                  ThrowStep(name),
                  Map("reason" -> "abrupt"),
                ) :: history,
                env,
              )
            case _ => optimizeCompletion(tail, x :: history, env)
          }
        case _ => optimizeCompletion(tail, x :: history, env)
      }
    case (x @ IfStep(_, thenStep, elseStep, _)) :: tail =>
      val thenStmt = optimizeCompletion(thenStep :: Nil, Nil, env)
      val elseStmt = elseStep.map(it => optimizeCompletion(it :: Nil, Nil, env))
      optimizeCompletion(
        tail,
        x.copy(
          thenStep = thenStmt.toBlockStep,
          elseStep = elseStmt.map(_.toBlockStep),
        ) :: history,
        env,
      ) */
    case head :: tail =>
      val (newStep, newEnv) = transformStep(head, env)
      newStep match {
        case Some(x) => optimizeCompletion(tail, x :: history, newEnv)
        case None => optimizeCompletion(tail, history, newEnv)
      }
    case Nil          => history.reverse
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

    case WrappedTryCatchStep(tryStep, catchVarRef @ Variable(catchVar, _), catchStep) =>
      val newTry = optimizeCompletion(tryStep :: Nil, Nil, env).toBlockStep

      val catchEnv = env + (catchVar -> AbruptCompletion)
      val newCatch = catchStep.map(c => optimizeCompletion(c :: Nil, Nil, catchEnv).toBlockStep)

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
      val elseStmt = ifStep.elseStep.map(step => optimizeCompletion(
        step :: Nil,
        Nil,
        env + (targetVar -> (if (checkType == "abrupt") NormalCompletion
        else AbruptCompletion)),
      ).toBlockStep)

      (Some(ifStep.copy(
        thenStep = bodyStmt,
        elseStep = elseStmt,
      )), env)

    case ret @ ReturnStep(
          ReturnIfAbruptExpression(ReferenceExpression(Variable(name, _)), true),
        ) =>
      env.get(name) match {
        case Some(AbruptCompletion) =>
          (Some(TaggedStep(ThrowStep(name), Map("reason" -> "abrupt"))), env)
        case _ => (Some(ret), env)
      }

    case BlockStep(stmts) =>
      (Some(optimizeCompletion(stmts.steps.map(_.step), Nil, env).toBlockStep), env)

    case IfStep(cond, t, e, cfg) =>
      val newT = optimizeCompletion(t :: Nil, Nil, env).toBlockStep
      val newE = e.map(b => optimizeCompletion(b :: Nil, Nil, env).toBlockStep)
      (Some(IfStep(cond, newT, newE, cfg)), env)

    case _ => (Some(step), env)
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
        val taggedCheck = annotateStep(check, "USE_FLAG", flagName)

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
