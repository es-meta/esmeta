package esmeta.lang

import esmeta.util.BaseUtils.*
import esmeta.spec.*
import esmeta.lang.*

class ParseAndStringifyTinyTest extends LangTest {
  val name: String = "langParseAndStringifyTest"

  // registration
  def init: Unit = {
    // pre-defined values

    // variables
    val x = Variable("x")

    // expressions
    val idExpr = IdentifierExpression(x)
    val lengthExpr = LengthExpression(idExpr)

    // conditions
    val exprCond = ExpressionCondition(idExpr)
    val binaryCond = BinaryCondition(idExpr, BinaryCondition.Op.Is, lengthExpr)
    val compCond =
      CompoundCondition(exprCond, CompoundCondition.Op.And, exprCond)

    // steps
    val letStep = LetStep(x, idExpr)

    // blocks
    val orderedBlock = Order(
      List(
        letStep,
        letStep,
        letStep,
      ),
    )
    val unOrderedBlock = Unorder(
      List(
        letStep,
        letStep,
        letStep,
      ),
    )
    val figureBlock = Figure(List("a", "b", "c"))

    // TODO handle upper case
    // // -----------------------------------------------------------------------------
    // // Block
    // // -----------------------------------------------------------------------------
    // checkString("Block", Block.apply)(
    //   orderedBlock -> s"""
    //   |  1. Let _x_ be _x_.
    //   |  1. Let _x_ be _x_.
    //   |  1. Let _x_ be _x_.
    //   |""".stripMargin,
    //   // unOrderedBlock -> s"""
    //   // |  * $letStepStr
    //   // |  * $letStepStr
    //   // |  * $letStepStr
    //   // |""".stripMargin,
    //   // figureBlock -> s"""
    //   // |  <figure>
    //   // |a
    //   // |b
    //   // |c
    //   // |  </figure>
    //   // """,
    // )

    // -----------------------------------------------------------------------------
    // Step
    // -----------------------------------------------------------------------------
    checkParseAndStringify("Step", Step.apply)(
      letStep -> "let _x_ be _x_.",
    )

    // -----------------------------------------------------------------------------
    // Expression
    // -----------------------------------------------------------------------------
    checkParseAndStringify("Expression", Expression.apply)(
      idExpr -> "_x_",
      lengthExpr -> "the length of _x_",
    )

    // -----------------------------------------------------------------------------
    // Condition
    // -----------------------------------------------------------------------------
    checkParseAndStringify("Condition", Condition.apply)(
      exprCond -> "_x_",
      binaryCond -> "_x_ is the length of _x_",
      compCond -> "_x_ and _x_",
    )

    // -----------------------------------------------------------------------------
    // Identifier
    // -----------------------------------------------------------------------------
    checkParseAndStringify("Identifier", Identifier.apply)(
      x -> "_x_",
    )
  }

  init
}
