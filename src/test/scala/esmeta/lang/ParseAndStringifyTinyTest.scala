package esmeta.lang

import esmeta.util.BaseUtils.*
import esmeta.spec.*
import esmeta.lang.*

class ParseAndStringifyTinyTest extends LangTest {
  val name: String = "langParseAndStringifyTest"

  // registration
  def init: Unit = {
    // -------------------------------------------------------------------------
    // Block
    // -------------------------------------------------------------------------
    def toBlockStep(steps: Step*): BlockStep =
      BlockStep(StepBlock(steps.toList.map(SubStep(None, _))))
    lazy val subStep = SubStep(None, letStep)
    lazy val subStepId = SubStep(Some("this-is-id"), letStep)
    lazy val stepBlock = StepBlock(List(subStep, subStepId, subStep))
    lazy val exprBlock = ExprBlock(List(refExpr, refExpr, refExpr))
    lazy val figureBlock = Figure(List("a", "b", "c"))

    // tests
    checkParseAndStringify("Block", Block)(
      stepBlock -> """
      |  1. Let _x_ be _x_.
      |  1. [id="this-is-id"] Let _x_ be _x_.
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

    // -------------------------------------------------------------------------
    // algorithm steps
    // -------------------------------------------------------------------------
    lazy val letStep = LetStep(x, refExpr)
    lazy val letStepClosure =
      LetStep(x, AbstractClosureExpression(List(x, x), List(x), blockStep))
    lazy val setStep = SetStep(x, addExpr)
    lazy val ifStep = IfStep(binaryCondLt, letStep, None)
    lazy val ifBlockStep =
      IfStep(binaryCondLt, blockStep, None)
    lazy val ifElseStep =
      IfStep(binaryCondLt, blockStep, Some(blockStep))
    lazy val ifElseIfStep =
      IfStep(binaryCondLt, blockStep, Some(ifBlockStep))
    lazy val ifElseIfElseStep =
      IfStep(binaryCondLt, blockStep, Some(ifElseStep))
    lazy val returnStep = ReturnStep(Some(refExpr))
    lazy val returnStepNoExpr = ReturnStep(None)
    lazy val assertStep = AssertStep(compCond)
    lazy val forEachStep = ForEachStep(Some(ty), x, refExpr, letStep)
    lazy val forEachStepNoType = ForEachStep(None, x, refExpr, letStep)
    lazy val forEachIntStepTrue =
      ForEachIntegerStep(x, refExpr, exprCond, true, letStep)
    lazy val forEachIntStepFalse =
      ForEachIntegerStep(x, refExpr, exprCond, false, letStep)
    lazy val throwStep = ThrowStep("TypeError")
    lazy val performStep = PerformStep(invokeAOExpr)
    lazy val appendStep = AppendStep(refExpr, fieldRef)
    lazy val repeatStep = RepeatStep(None, letStep)
    lazy val repeatCondStep = RepeatStep(Some(compCond), blockStep)
    lazy val pushStep = PushStep(x)
    lazy val noteStep = NoteStep(
      "At this point, it must be a numeric operation.",
    )
    lazy val suspendStep = SuspendStep(x)
    lazy val blockStep = BlockStep(StepBlock(List(SubStep(None, letStep))))
    lazy val yetStep = YetStep(yetExpr)

    // tests
    checkParseAndStringify("Step", Step)(
      letStep -> "let _x_ be _x_.",
      toBlockStep(letStepClosure) -> """
      |  1. Let _x_ be a new Abstract Closure with parameters (_x_, _x_) that captures _x_ and performs the following steps when called:
      |    1. Let _x_ be _x_.""".stripMargin,
      setStep -> "set _x_ to _x_ + _x_.",
      ifStep -> "if _x_ < _x_ + _x_, let _x_ be _x_.",
      toBlockStep(ifBlockStep) -> """
      |  1. If _x_ < _x_ + _x_, then
      |    1. Let _x_ be _x_.""".stripMargin,
      toBlockStep(ifElseStep) -> """
      |  1. If _x_ < _x_ + _x_, then
      |    1. Let _x_ be _x_.
      |  1. Else,
      |    1. Let _x_ be _x_.""".stripMargin,
      toBlockStep(ifElseIfStep) -> """
      |  1. If _x_ < _x_ + _x_, then
      |    1. Let _x_ be _x_.
      |  1. Else if _x_ < _x_ + _x_, then
      |    1. Let _x_ be _x_.""".stripMargin,
      toBlockStep(ifElseIfElseStep) -> """
      |  1. If _x_ < _x_ + _x_, then
      |    1. Let _x_ be _x_.
      |  1. Else if _x_ < _x_ + _x_, then
      |    1. Let _x_ be _x_.
      |  1. Else,
      |    1. Let _x_ be _x_.""".stripMargin,
      returnStep -> "return _x_.",
      returnStepNoExpr -> "return.",
      assertStep -> "assert: _x_ and _x_.",
      forEachStep -> "for each Object _x_ of _x_, let _x_ be _x_.",
      forEachStepNoType -> "for each _x_ of _x_, let _x_ be _x_.",
      forEachIntStepTrue -> (
        "for each integer _x_ starting with _x_ such that _x_, " +
          "in ascending order, let _x_ be _x_."
      ),
      forEachIntStepFalse -> (
        "for each integer _x_ starting with _x_ such that _x_, " +
          "in descending order, let _x_ be _x_."
      ),
      throwStep -> "throw a *TypeError* exception.",
      performStep -> "perform ToObject(_x_ + _x_, -_x_).",
      appendStep -> "append _x_ to _x_.[[Value]].",
      repeatStep -> "repeat, let _x_ be _x_.",
      repeatCondStep -> """repeat, while _x_ and _x_,
      |  1. Let _x_ be _x_.""".stripMargin,
      pushStep -> ("push _x_ onto the execution context stack; " +
        "_x_ is now the running execution context."),
      noteStep -> "NOTE: At this point, it must be a numeric operation.",
      suspendStep -> "suspend _x_.",
      blockStep -> """
      |  1. Let _x_ be _x_.""".stripMargin,
    )

    // -------------------------------------------------------------------------
    // algorithm expressions
    // -------------------------------------------------------------------------
    lazy val refExpr = ReferenceExpression(x)
    lazy val stringConcatExprOne =
      StringConcatExpression(List(refExpr))
    lazy val stringConcatExprTwo =
      StringConcatExpression(List(refExpr, refExpr))
    lazy val stringConcatExprThree =
      StringConcatExpression(List(refExpr, refExpr, refExpr))
    lazy val listConcatExprOne =
      ListConcatExpression(List(refExpr))
    lazy val listConcatExprTwo =
      ListConcatExpression(List(refExpr, refExpr))
    lazy val listConcatExprThree =
      ListConcatExpression(List(refExpr, refExpr, refExpr))
    lazy val recordEmptyExpr =
      RecordExpression(ty, Nil)
    lazy val recordExpr =
      RecordExpression(ty, List(field -> refExpr))
    lazy val lengthExpr = LengthExpression(refExpr)
    lazy val substrExpr = SubstringExpression(refExpr, refExpr, refExpr)
    lazy val numberOfExpr = NumberOfExpression(refExpr)
    lazy val sourceTextExpr = SourceTextExpression(nt)
    lazy val intrExpr = IntrinsicExpression(intr)
    lazy val invokeAOExpr =
      InvokeAbstractOperationExpression("ToObject", List(addExpr, unExpr))
    lazy val invokeNumericExpr =
      InvokeNumericMethodExpression(
        Type("Number"),
        "add",
        List(refExpr, refExpr),
      )
    lazy val invokeClosureExpr =
      InvokeAbstractClosureExpression(x, List(refExpr))
    lazy val invokeMethodExpr =
      InvokeMethodExpression(fieldRef, List(addExpr, unExpr))
    lazy val invokeSDOExprZero =
      InvokeSyntaxDirectedOperationExpression(nt, "StringValue", Nil)
    lazy val invokeSDOExprSingle =
      InvokeSyntaxDirectedOperationExpression(
        nt,
        "StringValue",
        List(nt),
      )
    lazy val invokeSDOExprMulti =
      InvokeSyntaxDirectedOperationExpression(
        nt,
        "StringValue",
        List(nt, refExpr),
      )
    lazy val invokeSDOExprEval =
      InvokeSyntaxDirectedOperationExpression(
        nt,
        "Evaluation",
        Nil,
      )
    lazy val invokeSDOExprContains =
      InvokeSyntaxDirectedOperationExpression(
        nt,
        "Contains",
        List(refExpr),
      )
    lazy val riaCheckExpr = ReturnIfAbruptExpression(invokeAOExpr, true)
    lazy val riaNoCheckExpr = ReturnIfAbruptExpression(invokeAOExpr, false)
    lazy val emptyListExpr = ListExpression(Nil)
    lazy val listExpr = ListExpression(List(refExpr, refExpr))
    lazy val yetExpr = YetExpression("Not yet supported:", Some(stepBlock))

    // tests
    checkParseAndStringify("Expression", Expression)(
      refExpr -> "_x_",
      stringConcatExprOne -> "the string-concatenation of _x_",
      stringConcatExprTwo -> "the string-concatenation of _x_ and _x_",
      stringConcatExprThree -> "the string-concatenation of _x_, _x_, and _x_",
      listConcatExprOne -> "the list-concatenation of _x_",
      listConcatExprTwo -> "the list-concatenation of _x_ and _x_",
      listConcatExprThree -> "the list-concatenation of _x_, _x_, and _x_",
      recordEmptyExpr -> "Object { }",
      recordExpr -> "Object { [[Value]]: _x_ }",
      lengthExpr -> "the length of _x_",
      substrExpr -> "the substring of _x_ from _x_ to _x_",
      numberOfExpr -> "the number of elements in _x_",
      sourceTextExpr -> "the source text matched by |Identifier|",
      intrExpr -> "%Array%",
      invokeAOExpr -> "ToObject(_x_ + _x_, -_x_)",
      invokeNumericExpr -> "Number::add(_x_, _x_)",
      invokeClosureExpr -> "_x_(_x_)",
      invokeMethodExpr -> "_x_.[[Value]](_x_ + _x_, -_x_)",
      invokeSDOExprZero -> "StringValue of |Identifier|",
      invokeSDOExprSingle -> ("StringValue of |Identifier| " +
        "using |Identifier| as the argument"),
      invokeSDOExprMulti -> ("StringValue of |Identifier| " +
        "using |Identifier| and _x_ as the arguments"),
      invokeSDOExprEval -> "the result of evaluating |Identifier|",
      invokeSDOExprContains -> "|Identifier| Contains _x_",
      riaCheckExpr -> "? ToObject(_x_ + _x_, -_x_)",
      riaNoCheckExpr -> "! ToObject(_x_ + _x_, -_x_)",
      emptyListExpr -> "« »",
      listExpr -> "« _x_, _x_ »",
    )

    // -------------------------------------------------------------------------
    // algorithm calcualation expressions
    // -------------------------------------------------------------------------
    lazy val minExpr =
      MathOpExpression(MathOpExpression.Op.Min, List(refExpr))
    lazy val toNumberExpr =
      MathOpExpression(MathOpExpression.Op.ToNumber, List(refExpr))
    lazy val addExpr =
      BinaryExpression(refExpr, BinaryExpression.Op.Add, refExpr)
    lazy val subExpr =
      BinaryExpression(refExpr, BinaryExpression.Op.Sub, refExpr)
    lazy val mulExpr =
      BinaryExpression(refExpr, BinaryExpression.Op.Mul, refExpr)
    lazy val expExpr =
      ExponentiationExpression(refExpr, refExpr)
    lazy val unExpr =
      UnaryExpression(UnaryExpression.Op.Neg, refExpr)
    lazy val parenAddExpr =
      BinaryExpression(refExpr, BinaryExpression.Op.Mul, addExpr)
    lazy val parenMulExpr =
      UnaryExpression(UnaryExpression.Op.Neg, mulExpr)
    lazy val parenUnExpr =
      ExponentiationExpression(unExpr, refExpr)

    // tests
    checkParseAndStringify("CalcExpression", Expression)(
      minExpr -> "min(_x_)",
      toNumberExpr -> "𝔽(_x_)",
      addExpr -> "_x_ + _x_",
      subExpr -> "_x_ - _x_",
      mulExpr -> "_x_ × _x_",
      expExpr -> "_x_<sup>_x_</sup>",
      unExpr -> "-_x_",
      parenAddExpr -> "_x_ × (_x_ + _x_)",
      parenMulExpr -> "-(_x_ × _x_)",
      parenUnExpr -> "(-_x_)<sup>_x_</sup>",
    )

    // -------------------------------------------------------------------------
    // algorithm literals
    // -------------------------------------------------------------------------
    lazy val hex = HexLiteral(0x0024, None)
    lazy val hexWithName = HexLiteral(0x0024, Some("DOLLAR SIGN"))
    lazy val code = CodeLiteral("|")
    lazy val nt = NonterminalLiteral(None, "Identifier")
    lazy val firstNt = NonterminalLiteral(Some(1), "Identifier")
    lazy val secondNt = NonterminalLiteral(Some(2), "Identifier")
    lazy val empty = ConstLiteral("empty")
    lazy val emptyStr = StringLiteral("")
    lazy val str = StringLiteral("abc")
    lazy val strWithStar = StringLiteral("abc*")
    lazy val strWithBasckSlash = StringLiteral("abc\\")
    lazy val fieldLit = FieldLiteral(field)
    lazy val sym = SymbolLiteral("iterator")
    lazy val mathVal = DecimalMathValueLiteral(BigDecimal("0.5"))
    lazy val posZero = NumberLiteral(+0.0)
    lazy val negZero = NumberLiteral(-0.0)
    lazy val posInf = NumberLiteral(Double.PositiveInfinity)
    lazy val negInf = NumberLiteral(Double.NegativeInfinity)
    lazy val nan = NumberLiteral(Double.NaN)
    lazy val number = NumberLiteral(1)
    lazy val bigint = BigIntLiteral(BigInt("1000000000000000000000000"))

    // tests
    checkParseAndStringify("Literal", Expression)(
      ThisLiteral -> "*this* value",
      NewTargetLiteral -> "NewTarget",
      hex -> "0x0024",
      hexWithName -> "0x0024 (DOLLAR SIGN)",
      code -> "`|`",
      nt -> "|Identifier|",
      firstNt -> "the first |Identifier|",
      secondNt -> "the second |Identifier|",
      empty -> "~empty~",
      emptyStr -> """*""*""",
      str -> """*"abc"*""",
      strWithStar -> """*"abc\*"*""",
      strWithBasckSlash -> """*"abc\\"*""",
      fieldLit -> "[[Value]]",
      sym -> "@@iterator",
      PositiveInfinityMathValueLiteral -> "+∞",
      NegativeInfinityMathValueLiteral -> "-∞",
      mathVal -> "0.5",
      posZero -> "*+0*<sub>𝔽</sub>",
      negZero -> "*-0*<sub>𝔽</sub>",
      posInf -> "*+∞*<sub>𝔽</sub>",
      negInf -> "*-∞*<sub>𝔽</sub>",
      nan -> "*NaN*",
      number -> "*1*<sub>𝔽</sub>",
      bigint -> "*1000000000000000000000000*<sub>ℤ</sub>",
      TrueLiteral -> "*true*",
      FalseLiteral -> "*false*",
      UndefinedLiteral -> "*undefined*",
      NullLiteral -> "*null*",
      AbsentLiteral -> "absent",
      UndefinedTypeLiteral -> "Undefined",
      NullTypeLiteral -> "Null",
      BooleanTypeLiteral -> "Boolean",
      StringTypeLiteral -> "String",
      SymbolTypeLiteral -> "Symbol",
      NumberTypeLiteral -> "Number",
      BigIntTypeLiteral -> "BigInt",
      ObjectTypeLiteral -> "Object",
    )

    // -------------------------------------------------------------------------
    // algorithm conditions
    // -------------------------------------------------------------------------
    lazy val exprCond = ExpressionCondition(refExpr)
    lazy val instanceOfCond = InstanceOfCondition(refExpr, false, ty)
    lazy val notInstanceOfCond = InstanceOfCondition(refExpr, true, ty)
    lazy val hasFieldCond = HasFieldCondition(refExpr, false, field)
    lazy val noHasFieldCond = HasFieldCondition(refExpr, true, field)
    lazy val abruptCond = AbruptCompletionCondition(x, false)
    lazy val isCond = IsAreCondition(List(refExpr), false, List(lengthExpr))
    lazy val areCond =
      IsAreCondition(List(refExpr, refExpr), true, List(TrueLiteral))
    lazy val isEitherCond =
      IsAreCondition(List(refExpr), false, List(TrueLiteral, FalseLiteral))
    lazy val isNeitherCond =
      IsAreCondition(List(refExpr), true, List(TrueLiteral, FalseLiteral))
    lazy val isPresentCond =
      IsAreCondition(List(refExpr), true, List(AbsentLiteral))
    lazy val binaryCondLt =
      BinaryCondition(refExpr, BinaryCondition.Op.LessThan, addExpr)
    lazy val compCond =
      CompoundCondition(exprCond, CompoundCondition.Op.And, exprCond)
    lazy val implyCond =
      CompoundCondition(isCond, CompoundCondition.Op.Imply, isEitherCond)
    checkParseAndStringify("Condition", Condition)(
      exprCond -> "_x_",
      instanceOfCond -> "_x_ is a Object",
      notInstanceOfCond -> "_x_ is not a Object",
      hasFieldCond -> "_x_ has a [[Value]] internal slot",
      noHasFieldCond -> "_x_ does not have a [[Value]] internal slot",
      abruptCond -> "_x_ is an abrupt completion",
      isCond -> "_x_ is the length of _x_",
      areCond -> "both _x_ and _x_ are not *true*",
      isEitherCond -> "_x_ is either *true* or *false*",
      isNeitherCond -> "_x_ is neither *true* nor *false*",
      isPresentCond -> "_x_ is present",
      binaryCondLt -> "_x_ < _x_ + _x_",
      compCond -> "_x_ and _x_",
      implyCond -> "If _x_ is the length of _x_, then _x_ is either *true* or *false*",
    )

    // -------------------------------------------------------------------------
    // algorithm references
    // -------------------------------------------------------------------------
    lazy val x = Variable("x")
    lazy val fieldRef = PropertyReference(x, fieldProp)
    lazy val intrFieldRef = PropertyReference(x, FieldProperty(intrField))
    lazy val propIntrFieldRef =
      PropertyReference(x, FieldProperty(propIntrField))
    lazy val componentRef = PropertyReference(x, componentProp)
    lazy val indexRef = PropertyReference(x, indexProp)

    // tests
    checkParseAndStringify("Reference", Reference)(
      x -> "_x_",
      RunningExecutionContext -> "the running execution context",
      CurrentRealmRecord -> "the current Realm Record",
      ActiveFunctionObject -> "the active function object",
      fieldRef -> "_x_.[[Value]]",
      intrFieldRef -> "_x_.[[%Array%]]",
      propIntrFieldRef -> "_x_.[[%Array.prototype.toString%]]",
      componentRef -> "_x_.Realm",
      indexRef -> "_x_[_x_]",
    )

    // -------------------------------------------------------------------------
    // algorithm references
    // -------------------------------------------------------------------------
    lazy val fieldProp = FieldProperty(field)
    lazy val componentProp = ComponentProperty("Realm")
    lazy val indexProp = IndexProperty(refExpr)

    // tests
    checkParseAndStringify("Property", Property)(
      fieldProp -> ".[[Value]]",
      componentProp -> ".Realm",
      indexProp -> "[_x_]",
    )

    // -------------------------------------------------------------------------
    // algorithm fields
    // -------------------------------------------------------------------------
    lazy val field = StringField("Value")
    lazy val intrField = IntrinsicField(intr)
    lazy val propIntrField = IntrinsicField(propIntr)

    // tests
    checkParseAndStringify("Field", Field)(
      field -> "[[Value]]",
      intrField -> "[[%Array%]]",
      propIntrField -> "[[%Array.prototype.toString%]]",
    )

    // -------------------------------------------------------------------------
    // algorithm intrinsics
    // -------------------------------------------------------------------------
    lazy val intr = Intrinsic("Array", Nil)
    lazy val propIntr = Intrinsic("Array", List("prototype", "toString"))

    // tests
    checkParseAndStringify("Intrinsic", Intrinsic)(
      intr -> "%Array%",
      propIntr -> "%Array.prototype.toString%",
    )

    // -------------------------------------------------------------------------
    // algorithm types
    // -------------------------------------------------------------------------
    lazy val ty = Type("Object")

    // tests
    checkParseAndStringify("Type", Type)(
      ty -> "Object",
    )
  }

  init
}
