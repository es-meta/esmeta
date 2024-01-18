package esmeta.lang

import esmeta.ESMetaTest

/** test for metalangauge */
trait LangTest extends ESMetaTest {
  def category: String = "lang"
}
object LangTest {
  // blocks
  def toBlockStep(steps: Step*): BlockStep =
    BlockStep(StepBlock(steps.toList.map(SubStep(None, _))))
  lazy val subStep = SubStep(None, letStep)
  lazy val directive = Directive("id", List("x", "y", "z"))
  lazy val subStepId = SubStep(Some(directive), letStep)
  lazy val stepBlock = StepBlock(List(subStep, subStepId, subStep))
  lazy val exprBlock = ExprBlock(List(refExpr, refExpr, refExpr))
  lazy val figureBlock = Figure(List("a", "b", "c"))

  // algorithm steps
  lazy val letStep = LetStep(x, refExpr)
  lazy val letStepClosure =
    LetStep(x, AbstractClosureExpression(List(x, x), List(x), blockStep))
  lazy val setStep = SetStep(x, addExpr)
  lazy val setFieldsWithIntrinsicsStep = SetFieldsWithIntrinsicsStep(x)
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
  lazy val forEachStep = ForEachStep(Some(ty), x, refExpr, true, letStep)
  lazy val forEachReverseStep =
    ForEachStep(Some(ty), x, refExpr, false, letStep)
  lazy val forEachStepNoType =
    ForEachStep(None, x, refExpr, true, letStep)
  lazy val forEachIntStepTrue = ForEachIntegerStep(
    x,
    DecimalMathValueLiteral(BigDecimal(2)),
    DecimalMathValueLiteral(BigDecimal(5)),
    true,
    letStep,
  )
  lazy val forEachIntStepFalse = ForEachIntegerStep(
    x,
    DecimalMathValueLiteral(BigDecimal(2)),
    DecimalMathValueLiteral(BigDecimal(5)),
    false,
    letStep,
  )
  lazy val forEachArrayIndexStep =
    ForEachArrayIndexStep(x, x, refExpr, false, blockStep)
  lazy val forEachAscOPKStep = ForEachOwnPropertyKeyStep(
    x,
    x,
    compCond,
    true,
    ForEachOwnPropertyKeyStepOrder.NumericIndexOrder,
    letStep,
  )
  lazy val forEachDscOPKStep = ForEachOwnPropertyKeyStep(
    x,
    x,
    compCond,
    false,
    ForEachOwnPropertyKeyStepOrder.ChronologicalOrder,
    letStep,
  )
  lazy val throwStep = ThrowStep(errObj)
  lazy val performStep = PerformStep(invokeAOExpr)
  lazy val appendStep = AppendStep(refExpr, fieldRef)
  lazy val prependStep = PrependStep(refExpr, fieldRef)
  lazy val repeatStep = RepeatStep(None, letStep)
  lazy val repeatCondStep = RepeatStep(Some(compCond), blockStep)
  lazy val pushCtxtStep = PushCtxtStep(x)
  lazy val noteStep = NoteStep(
    "At this point, it must be a numeric operation.",
  )
  lazy val suspendStep = SuspendStep(x, false)
  lazy val suspendAndRemoveStep = SuspendStep(x, true)
  lazy val removeElemStep = RemoveElemStep(refExpr, refExpr)
  lazy val removeFirstStep = RemoveFirstStep(refExpr)
  lazy val removeCtxtStep = RemoveContextStep(x, None)
  lazy val removeCtxtWithRestoreStep = RemoveContextStep(x, Some(x))
  lazy val setEvalStateStep = SetEvaluationStateStep(x, None, blockStep)
  lazy val setEvalStateParamStep =
    SetEvaluationStateStep(x, Some(x), blockStep)
  lazy val resumeStep = ResumeEvaluationStep(x, None, None, List(subStep))
  lazy val resumeArgStep =
    ResumeEvaluationStep(x, Some(refExpr), None, List(subStep))
  lazy val resumeParamStep =
    ResumeEvaluationStep(x, None, Some(x), List(subStep))
  lazy val resumeYieldStep =
    ResumeYieldStep(x, refExpr, x, x, List(subStep))
  lazy val returnToResumeStep =
    ReturnToResumeStep(x, returnStep)
  lazy val blockStep = BlockStep(StepBlock(List(SubStep(None, letStep))))
  lazy val yetStep = YetStep(yetExpr)

  // algorithm expressions
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
    RecordExpression("Object", Nil)
  lazy val recordExpr =
    RecordExpression("Object", List(fieldLit -> refExpr))
  lazy val lengthExpr = LengthExpression(refExpr)
  lazy val substrExpr = SubstringExpression(refExpr, refExpr, None)
  lazy val substrExprTo = SubstringExpression(refExpr, refExpr, Some(refExpr))
  lazy val trim = TrimExpression(refExpr, true, true)
  lazy val trimStart = TrimExpression(refExpr, true, false)
  lazy val trimEnd = TrimExpression(refExpr, false, true)
  lazy val numberOfExpr = NumberOfExpression(refExpr)
  lazy val sourceTextExpr = SourceTextExpression(nt)
  lazy val coveredByExpr = CoveredByExpression(nt, nt)
  lazy val getItemsExpr = GetItemsExpression(nt, refExpr)
  lazy val intrExpr = IntrinsicExpression(intr)
  lazy val invokeAOExpr =
    InvokeAbstractOperationExpression("ToObject", List(addExpr, unExpr))
  lazy val invokeNumericExpr =
    InvokeNumericMethodExpression(
      "Number",
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
  lazy val xrefAlgoExpr = XRefExpression(XRefExpressionOperator.Algo, "sec-x")
  lazy val xrefSlotsExpr =
    XRefExpression(XRefExpressionOperator.InternalSlots, "sec-x")
  lazy val xrefLenExpr =
    XRefExpression(XRefExpressionOperator.ParamLength, "sec-x")
  lazy val soleExpr = SoleElementExpression(listExpr)
  lazy val codeUnitAtExpr =
    CodeUnitAtExpression(refExpr, refExpr)
  lazy val yetExpr = YetExpression("Not yet supported:", Some(stepBlock))

  // algorithm calcualation expressions
  lazy val minExpr =
    MathFuncExpression(MathFuncExpressionOperator.Min, List(refExpr))
  lazy val addExpr =
    BinaryExpression(refExpr, BinaryExpressionOperator.Add, refExpr)
  lazy val subExpr =
    BinaryExpression(refExpr, BinaryExpressionOperator.Sub, refExpr)
  lazy val mulExpr =
    BinaryExpression(refExpr, BinaryExpressionOperator.Mul, refExpr)
  lazy val expExpr =
    ExponentiationExpression(refExpr, refExpr)
  lazy val unExpr =
    UnaryExpression(UnaryExpressionOperator.Neg, refExpr)
  lazy val parenAddExpr =
    BinaryExpression(refExpr, BinaryExpressionOperator.Mul, addExpr)
  lazy val parenMulExpr =
    UnaryExpression(UnaryExpressionOperator.Neg, mulExpr)
  lazy val parenUnExpr =
    ExponentiationExpression(unExpr, refExpr)
  lazy val convToApproxNumberExpr =
    ConversionExpression(
      ConversionExpressionOperator.ToApproxNumber,
      refExpr,
    )
  lazy val convToNumberTextExpr =
    ConversionExpression(
      ConversionExpressionOperator.ToNumber,
      codeUnitAtExpr,
    )
  lazy val convToBigIntTextExpr =
    ConversionExpression(
      ConversionExpressionOperator.ToBigInt,
      codeUnitAtExpr,
    )
  lazy val convToMathTextExpr =
    ConversionExpression(ConversionExpressionOperator.ToMath, codeUnitAtExpr)
  lazy val convToNumberExpr =
    ConversionExpression(ConversionExpressionOperator.ToNumber, refExpr)
  lazy val convToBigIntExpr =
    ConversionExpression(ConversionExpressionOperator.ToBigInt, refExpr)
  lazy val convToMathExpr =
    ConversionExpression(ConversionExpressionOperator.ToMath, refExpr)

  // algorithm literals
  lazy val hex = HexLiteral(0x0024, None)
  lazy val hexWithName = HexLiteral(0x0024, Some("DOLLAR SIGN"))
  lazy val code = CodeLiteral("|")
  lazy val nt = NonterminalLiteral(None, "Identifier", List())
  lazy val firstNt = NonterminalLiteral(Some(1), "Identifier", List())
  lazy val secondNt = NonterminalLiteral(Some(2), "Identifier", List())
  lazy val ntFlags = NonterminalLiteral(None, "A", List("~Yield", "+Await"))
  lazy val empty = ConstLiteral("empty")
  lazy val emptyStr = StringLiteral("")
  lazy val str = StringLiteral("abc")
  lazy val strWithStar = StringLiteral("abc*")
  lazy val strWithBasckSlash = StringLiteral("abc\\")
  lazy val fieldLit = FieldLiteral("Value")
  lazy val sym = SymbolLiteral("iterator")
  lazy val errObj = ErrorObjectLiteral("TypeError")
  lazy val mathVal = DecimalMathValueLiteral(BigDecimal("0.5"))
  lazy val mathPi = MathConstantLiteral(1, "π")
  lazy val mathPiWithPre = MathConstantLiteral(2, "π")
  lazy val posZero = NumberLiteral(+0.0)
  lazy val negZero = NumberLiteral(-0.0)
  lazy val posInf = NumberLiteral(Double.PositiveInfinity)
  lazy val negInf = NumberLiteral(Double.NegativeInfinity)
  lazy val nan = NumberLiteral(Double.NaN)
  lazy val number = NumberLiteral(1)
  lazy val bigint = BigIntLiteral(BigInt("1000000000000000000000000"))

  // algorithm clamp expressions
  lazy val clampExpr = ClampExpression(refExpr, refExpr, refExpr)

  // algorithm bitwise expressions
  lazy val bAndExpr =
    BitwiseExpression(refExpr, BitwiseExpressionOperator.BAnd, refExpr)
  lazy val bXorExpr =
    BitwiseExpression(refExpr, BitwiseExpressionOperator.BXOr, refExpr)
  lazy val bOrExpr =
    BitwiseExpression(refExpr, BitwiseExpressionOperator.BOr, refExpr)

  // algorithm conditions
  lazy val exprCond = ExpressionCondition(refExpr)
  lazy val instanceOfCond = InstanceOfCondition(refExpr, false, List(ty))
  lazy val notInstanceOfCond = InstanceOfCondition(refExpr, true, List(ty))
  lazy val eitherInstanceOfCond =
    InstanceOfCondition(refExpr, false, List(ty, ty, ty))
  lazy val neitherInstanceOfCond =
    InstanceOfCondition(refExpr, true, List(ty, ty))
  lazy val hasFieldCond = HasFieldCondition(x, false, fieldLit)
  lazy val noHasFieldCond = HasFieldCondition(x, true, fieldLit)
  lazy val hasBindingCond = HasBindingCondition(x, false, refExpr)
  lazy val noHasBindingCond = HasBindingCondition(x, true, refExpr)
  lazy val prodCond = ProductionCondition(nt, "Identifier", "Identifier")
  lazy val finiteCond =
    PredicateCondition(refExpr, false, PredicateConditionOperator.Finite)
  lazy val abruptCond =
    PredicateCondition(refExpr, false, PredicateConditionOperator.Abrupt)
  lazy val normalCond =
    PredicateCondition(refExpr, false, PredicateConditionOperator.Normal)
  lazy val dupCond =
    PredicateCondition(refExpr, false, PredicateConditionOperator.Duplicated)
  lazy val presentCond =
    PredicateCondition(refExpr, false, PredicateConditionOperator.Present)
  lazy val emptyCond =
    PredicateCondition(refExpr, false, PredicateConditionOperator.Empty)
  lazy val strictCond =
    PredicateCondition(refExpr, false, PredicateConditionOperator.StrictMode)
  lazy val arrayIndexCond =
    PredicateCondition(refExpr, false, PredicateConditionOperator.ArrayIndex)
  lazy val nonNegativeCond =
    PredicateCondition(refExpr, false, PredicateConditionOperator.NonNegative)
  lazy val integralCond =
    PredicateCondition(
      refExpr,
      false,
      PredicateConditionOperator.IntegralNumber,
    )
  lazy val isCond = IsAreCondition(List(refExpr), false, List(lengthExpr))
  lazy val areCond =
    IsAreCondition(List(refExpr, refExpr), true, List(TrueLiteral()))
  lazy val isEitherCond =
    IsAreCondition(List(refExpr), false, List(TrueLiteral(), FalseLiteral()))
  lazy val isNeitherCond =
    IsAreCondition(List(refExpr), true, List(TrueLiteral(), FalseLiteral()))
  lazy val binaryCondLt =
    BinaryCondition(refExpr, BinaryConditionOperator.LessThan, addExpr)
  lazy val inclusiveIntervalCond =
    InclusiveIntervalCondition(
      refExpr,
      false,
      DecimalMathValueLiteral(BigDecimal(2)),
      DecimalMathValueLiteral(BigDecimal(32)),
    )
  lazy val notInclusiveIntervalCond =
    inclusiveIntervalCond.copy(negation = true)
  lazy val containsWhoseCond =
    ContainsWhoseCondition(refExpr, ty, "Value", refExpr)
  lazy val compCond =
    CompoundCondition(exprCond, CompoundConditionOperator.And, exprCond)
  lazy val implyCond =
    CompoundCondition(isCond, CompoundConditionOperator.Imply, isEitherCond)

  // algorithm references
  lazy val x = Variable("x")
  lazy val fieldRef = PropertyReference(x, fieldProp)
  lazy val intrFieldRef = PropertyReference(x, intrProp)
  lazy val propIntrFieldRef =
    PropertyReference(x, propIntrProp)
  lazy val componentRef = PropertyReference(x, componentProp)
  lazy val indexRef = PropertyReference(x, indexProp)
  lazy val ntRef = PropertyReference(x, NonterminalProperty("Arguments"))

  // algorithm references
  lazy val fieldProp = FieldProperty("Value")
  lazy val componentProp = ComponentProperty("Realm")
  lazy val bindingProp = BindingProperty(refExpr)
  lazy val indexProp = IndexProperty(refExpr)
  lazy val intrProp = IntrinsicProperty(intr)
  lazy val propIntrProp = IntrinsicProperty(propIntr)

  // algorithm intrinsics
  lazy val intr = Intrinsic("Array", Nil)
  lazy val propIntr = Intrinsic("Array", List("prototype", "toString"))

  // algorithm types
  lazy val ty = UnknownType("Base")
}
