package esmeta.lang

import esmeta.util.BaseUtils.*
import esmeta.spec.*
import esmeta.lang.*

class ParseAndStringifyTinyTest extends LangTest {
  val name: String = "langParseAndStringifyTest"

  // registration
  def init: Unit = {
    // pre-defined values

    // references
    lazy val x = Variable("x")
    lazy val field = Field(x, "Value")

    // expressions
    lazy val refExpr = ReferenceExpression(x)
    lazy val lengthExpr = LengthExpression(refExpr)
    lazy val substrExpr = SubstringExpression(refExpr, refExpr, refExpr)
    lazy val addExpr =
      BinaryExpression(refExpr, BinaryExpression.Op.Add, refExpr)
    lazy val subExpr =
      BinaryExpression(refExpr, BinaryExpression.Op.Sub, refExpr)
    lazy val mulExpr =
      BinaryExpression(refExpr, BinaryExpression.Op.Mul, refExpr)
    lazy val unExpr = UnaryExpression(UnaryExpression.Op.Neg, refExpr)
    lazy val invokeAOExpr =
      InvokeAbstractOperationExpression("ToObject", List(addExpr, unExpr))
    lazy val invokeSDOExprZero =
      InvokeSyntaxDirectedOperationExpression(ntExpr, "StringValue", Nil)
    lazy val invokeSDOExprSingle =
      InvokeSyntaxDirectedOperationExpression(
        ntExpr,
        "StringValue",
        List(ntExpr),
      )
    lazy val invokeSDOExprMulti =
      InvokeSyntaxDirectedOperationExpression(
        ntExpr,
        "StringValue",
        List(ntExpr, refExpr),
      )
    lazy val riaCheckExpr = ReturnIfAbruptExpression(invokeAOExpr, true)
    lazy val riaNoCheckExpr = ReturnIfAbruptExpression(invokeAOExpr, false)
    lazy val listExpr = ListExpression(List(refExpr, refExpr))
    lazy val ntExpr = NonterminalExpression("Identifier")
    lazy val yetExpr = YetExpression("todo", Some(stepBlock))

    // conditions
    lazy val exprCond = ExpressionCondition(refExpr)
    lazy val hasFieldCond = HasFieldCondition(refExpr, "Type")
    lazy val binaryCondIs =
      BinaryCondition(refExpr, BinaryCondition.Op.Is, lengthExpr)
    lazy val binaryCondLt =
      BinaryCondition(refExpr, BinaryCondition.Op.LessThan, addExpr)
    lazy val compCond =
      CompoundCondition(exprCond, CompoundCondition.Op.And, exprCond)

    // steps
    lazy val letStep = LetStep(x, refExpr)
    lazy val setStep = SetStep(x, addExpr)
    lazy val ifStep = IfStep(binaryCondIs, letStep, None)
    lazy val returnStep = ReturnStep(refExpr)
    lazy val assertStep = AssertStep(compCond)
    lazy val forEachIntStepTrue =
      ForEachIntegerStep(x, refExpr, exprCond, true, letStep)
    lazy val forEachIntStepFalse =
      ForEachIntegerStep(x, refExpr, exprCond, false, letStep)
    lazy val throwStep = ThrowStep("TypeError")
    lazy val performStep = PerformStep(invokeAOExpr)
    lazy val yetStep = YetStep(yetExpr)

    // blocks
    lazy val stepBlock = StepBlock(List(letStep, letStep, letStep))
    lazy val exprBlock = ExprBlock(List(refExpr, refExpr, refExpr))
    lazy val figureBlock = Figure(List("a", "b", "c"))

    // -----------------------------------------------------------------------------
    // Block
    // -----------------------------------------------------------------------------
    checkParseAndStringify("Block", Block.apply)(
      stepBlock -> """
      |  1. Let _x_ be _x_.
      |  1. Let _x_ be _x_.
      |  1. Let _x_ be _x_.""".stripMargin,
      exprBlock -> """
      |  * _x_
      |  * _x_
      |  * _x_""".stripMargin,
      figureBlock -> """
      |  <figure>
      |    a
      |    b
      |    c
      |  </figure>""".stripMargin,
    )

    // -----------------------------------------------------------------------------
    // Step
    // -----------------------------------------------------------------------------
    checkParseAndStringify("Step", Step.apply)(
      letStep -> "let _x_ be _x_.",
      setStep -> "set _x_ to _x_ + _x_.",
      ifStep -> "if _x_ is the length of _x_, let _x_ be _x_.",
      returnStep -> "return _x_.",
      assertStep -> "assert: _x_ and _x_.",
      forEachIntStepTrue -> "for each integer _x_ starting with _x_ such that _x_, in ascending order, let _x_ be _x_.",
      forEachIntStepFalse -> "for each integer _x_ starting with _x_ such that _x_, in descending order, let _x_ be _x_.",
      throwStep -> "throw a *TypeError* exception.",
      performStep -> "perform ToObject(_x_ + _x_, -_x_).",
      yetStep -> """[YET] todo
      |  1. Let _x_ be _x_.
      |  1. Let _x_ be _x_.
      |  1. Let _x_ be _x_.""".stripMargin,
    )

    // -----------------------------------------------------------------------------
    // Expression
    // -----------------------------------------------------------------------------
    checkParseAndStringify("Expression", Expression.apply)(
      refExpr -> "_x_",
      lengthExpr -> "the length of _x_",
      substrExpr -> "the substring of _x_ from _x_ to _x_",
      addExpr -> "_x_ + _x_",
      subExpr -> "_x_ - _x_",
      mulExpr -> "_x_ × _x_",
      unExpr -> "-_x_",
      invokeAOExpr -> "ToObject(_x_ + _x_, -_x_)",
      invokeSDOExprZero -> "StringValue of |Identifier|",
      invokeSDOExprSingle -> "StringValue of |Identifier| using |Identifier| as the argument",
      invokeSDOExprMulti -> "StringValue of |Identifier| using |Identifier| and _x_ as the arguments",
      riaCheckExpr -> "? ToObject(_x_ + _x_, -_x_)",
      riaNoCheckExpr -> "! ToObject(_x_ + _x_, -_x_)",
      ListExpression(Nil) -> "« »",
      listExpr -> "« _x_, _x_ »",
      ntExpr -> "|Identifier|",
      ThisLiteral -> "*this* value",
      ConstLiteral("empty") -> "~empty~",
      StringLiteral("") -> "*\"\"*",
      StringLiteral("abc") -> "*\"abc\"*",
      PositiveInfinityMathValueLiteral -> "+∞",
      NegativeInfinityMathValueLiteral -> "-∞",
      DecimalMathValueLiteral(BigDecimal("0.5")) -> "0.5",
      NumberLiteral(+0.0) -> "*+0*<sub>𝔽</sub>",
      NumberLiteral(-0.0) -> "*-0*<sub>𝔽</sub>",
      NumberLiteral(Double.PositiveInfinity) -> "*+∞*<sub>𝔽</sub>",
      NumberLiteral(Double.NegativeInfinity) -> "*-∞*<sub>𝔽</sub>",
      NumberLiteral(Double.NaN) -> "*NaN*",
      NumberLiteral(1) -> "*1*<sub>𝔽</sub>",
      BigIntLiteral(
        BigInt("1000000000000000000000000"),
      ) -> "*1000000000000000000000000*<sub>ℤ</sub>",
      TrueLiteral -> "*true*",
      FalseLiteral -> "*false*",
      UndefinedLiteral -> "*undefined*",
      NullLiteral -> "*null*",
    )

    // -----------------------------------------------------------------------------
    // Condition
    // -----------------------------------------------------------------------------
    checkParseAndStringify("Condition", Condition.apply)(
      exprCond -> "_x_",
      hasFieldCond -> "_x_ has a [[Type]] internal slot",
      binaryCondIs -> "_x_ is the length of _x_",
      binaryCondLt -> "_x_ < _x_ + _x_",
      compCond -> "_x_ and _x_",
    )

    // -----------------------------------------------------------------------------
    // Reference
    // -----------------------------------------------------------------------------
    checkParseAndStringify("Reference", Reference.apply)(
      x -> "_x_",
      field -> "_x_.[[Value]]",
    )
  }

  init
}
