package esmeta.es.util
import esmeta.lang.*

import scala.annotation.tailrec

private type Completion = "normal" | "abrupt"

object PolyfillInspector {

  def transform(step: Step): Step = step match {
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
    @tailrec
    def recurse(
      input: List[Step],
      history: List[Step],
      handledVars: Set[String],
    ): List[Step] = input match {
      case (check @ CompletionCheckPattern(checks)) :: tail =>
        val (checkType, targetVar) = checks

        if (handledVars.contains(targetVar)) {
          val flagName = s"${targetVar}_isAbrupt"
          val taggedCheck = annotateStep(check, "USE_FLAG", flagName)

          println(s"$checks : $check (reassigned: $checkType)")
          recurse(tail, taggedCheck :: history, handledVars)
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

            if (gap.isEmpty) {
              // No gap between checking; immediately wrap with try/catch
              val merged =
                mergeWithFlag(producer, check, targetVar, s"_${targetVar}_err", flagName, checkType)
              recurse(
                tail,
                merged :: flagDecl :: newHistory,
                handledVars + targetVar,
              )
            } else {
              // There are some gap between checking; just set the flag
              val wrappedProducer =
                wrapProducerOnly(producer, targetVar, s"_${targetVar}_err", flagName)
              val taggedCheck = annotateStep(check, "USE_FLAG", flagName)
              recurse(
                tail,
                flagDecl :: wrappedProducer :: gap ::: taggedCheck :: newHistory,
                handledVars + targetVar,
              )
            }
          } else {
            // This variable must be parameter
            recurse(tail, check :: history, handledVars)
          }
        }
      case head :: tail => recurse(tail, head :: history, handledVars)
      case Nil          => history.reverse
    }

    recurse(stmts, Nil, Set())
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
      val catchStmts = List(
        SetStep(
          Variable(varName, None),
          ReferenceExpression(Variable(catchVar, None)),
        ),
        SetStep(Variable(flagName, None), TrueLiteral()),
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
