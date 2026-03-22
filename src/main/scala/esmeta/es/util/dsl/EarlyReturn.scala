package esmeta.es.util.dsl

import esmeta.lang.*
import esmeta.lang.util.{Walker => LangWalker, UnitWalker => LangUnitWalker}

import scala.collection.mutable

/** Early-return wrapping for closure bodies.
  *
  * When a loop body is converted into a closure (e.g., ForEach → closure call),
  * `return` statements inside the body need special handling:
  *   1. Wrap each `ReturnStep(expr)` as `ReturnStep(Record{Type:
  *      "early-return", Value: expr})` 2. After the closure call, check if the
  *      result is an early-return and propagate it.
  */
object EarlyReturn {

  private val resultVariable = Variable("_result")

  /** Wrap all ReturnSteps in the body with early-return records. */
  def patchReturns(body: Step): Step = {
    new LangWalker {
      override def walk(step: Step): Step = step match {
        case ReturnStep(expr) =>
          ReturnStep(
            RecordExpression(
              "",
              List(
                (FieldLiteral("Type"), EnumLiteral("early-return")),
                (FieldLiteral("Value"), expr),
              ),
              RecordExpressionForm.SyntaxLiteral(None),
            ),
          )
        case _ => super.walk(step)
      }
    }.walk(body)
  }

  /** Generate the full early-return wrapped output:
    *   1. `Let _result = aoName(iterBase, closure(elementVar, patchedBody))` 2.
    *      `If _result !== undefined and _result.Type === "early-return", Return
    *      _result.Value`
    */
  def wrap(
    body: Step,
    aoName: String,
    iterBase: Reference,
    elementVar: String,
    subrules: List[Rule],
    ctx: DSLContext,
    stats: Option[TransformStats],
  ): List[Step] = {
    // Apply subrules to body first, then patch returns
    val transformedBody = subrules.foldLeft(body) { (s, rule) =>
      Transformer.transformStep(rule, s, ctx, stats)
    }
    val patchedBody = patchReturns(transformedBody)

    val closureCall = LetStep(
      resultVariable,
      InvokeAbstractOperationExpression(
        aoName,
        List(
          ReferenceExpression(iterBase),
          AbstractClosureExpression(
            List(Variable(elementVar)),
            List(),
            patchedBody,
          ),
        ),
        HtmlTag.None,
      ),
    )

    val earlyReturnCheck = IfStep(
      CompoundCondition(
        BinaryCondition(
          ReferenceExpression(resultVariable),
          BinaryConditionOperator.NEq,
          UndefinedLiteral(),
        ),
        CompoundConditionOperator.And,
        BinaryCondition(
          ReferenceExpression(
            Access(
              resultVariable,
              "Type",
              AccessKind.Field,
              AccessForm.Dot,
            ),
          ),
          BinaryConditionOperator.Eq,
          EnumLiteral("early-return"),
        ),
      ),
      ReturnStep(
        ReferenceExpression(
          Access(
            resultVariable,
            "Value",
            AccessKind.Field,
            AccessForm.Dot,
          ),
        ),
      ),
      None,
      IfStep.ElseConfig(),
    )

    List(closureCall, earlyReturnCheck)
  }
}
