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
    val x = Variable("x")
    val field = Field(x, "Value")

    // expressions
    val refExpr = ReferenceExpression(x)
    val lengthExpr = LengthExpression(refExpr)
    val substrExpr = SubstringExpression(refExpr, refExpr, refExpr)
    val addExpr = BinaryExpression(refExpr, BinaryExpression.Op.Add, refExpr)
    val subExpr = BinaryExpression(refExpr, BinaryExpression.Op.Sub, refExpr)
    val mulExpr = BinaryExpression(refExpr, BinaryExpression.Op.Mul, refExpr)
    val unExpr = UnaryExpression(UnaryExpression.Op.Neg, refExpr)
    val invokeExpr = InvokeExpression("ToObject", List(addExpr, unExpr))
    val riaCheckExpr = ReturnIfAbruptExpression(invokeExpr, true)
    val riaNoCheckExpr = ReturnIfAbruptExpression(invokeExpr, false)
    val listExpr = ListExpression(List(refExpr, refExpr))
    val ntExpr = NonterminalExpression("Identifier")

    // conditions
    val exprCond = ExpressionCondition(refExpr)
    val hasFieldCond = HasFieldCondition(refExpr, "Type")
    val binaryCondIs =
      BinaryCondition(refExpr, BinaryCondition.Op.Is, lengthExpr)
    val binaryCondLt =
      BinaryCondition(refExpr, BinaryCondition.Op.LessThan, addExpr)
    val compCond =
      CompoundCondition(exprCond, CompoundCondition.Op.And, exprCond)

    // steps
    val letStep = LetStep(x, refExpr)
    val setStep = SetStep(x, addExpr)
    val ifStep = IfStep(binaryCondIs, letStep, None)
    val returnStep = ReturnStep(refExpr)
    val assertStep = AssertStep(compCond)
    val forEachIntStepTrue =
      ForEachIntegerStep(x, refExpr, exprCond, true, letStep)
    val forEachIntStepFalse =
      ForEachIntegerStep(x, refExpr, exprCond, false, letStep)
    val throwStep = ThrowStep("TypeError")
    val performStep = PerformStep(invokeExpr)

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
      setStep -> "set _x_ to _x_ + _x_.",
      ifStep -> "if _x_ is the length of _x_, let _x_ be _x_.",
      returnStep -> "return _x_.",
      assertStep -> "assert: _x_ and _x_.",
      forEachIntStepTrue -> "for each integer _x_ starting with _x_ such that _x_, in ascending order, let _x_ be _x_.",
      forEachIntStepFalse -> "for each integer _x_ starting with _x_ such that _x_, in descending order, let _x_ be _x_.",
      throwStep -> "throw a *TypeError* exception.",
      performStep -> "perform ToObject(_x_ + _x_, -_x_).",
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
      invokeExpr -> "ToObject(_x_ + _x_, -_x_)",
      riaCheckExpr -> "? ToObject(_x_ + _x_, -_x_)",
      riaNoCheckExpr -> "! ToObject(_x_ + _x_, -_x_)",
      ListExpression(Nil) -> "« »",
      listExpr -> "« _x_, _x_ »",
      ntExpr -> "|Identifier|",
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
