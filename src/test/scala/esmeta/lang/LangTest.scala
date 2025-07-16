package esmeta.lang

import esmeta.ESMetaTest
import esmeta.spec
import esmeta.ty.*
import scala.collection.mutable.{Set as MSet}

/** test for metalangauge */
trait LangTest extends ESMetaTest {
  def category: String = "lang"
}
object LangTest {
  val T = true
  val F = false

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
  lazy val setAsStep = SetAsStep(x, "specified", "id")
  lazy val setEvalStateStep = SetEvaluationStateStep(x, x, Nil)
  lazy val setEvalStateArgStep = SetEvaluationStateStep(x, x, List(refExpr))
  lazy val setEvalStateArgsStep =
    SetEvaluationStateStep(x, x, List(refExpr, refExpr))
  lazy val performStep = PerformStep(invokeAOExpr)
  lazy val invokeShorthandStep = InvokeShorthandStep(
    "IfAbruptCloseIterator",
    List(refExpr, refExpr),
  )
  lazy val appendStep = AppendStep(refExpr, fieldRef)
  lazy val prependStep = PrependStep(refExpr, fieldRef)
  lazy val addStep = AddStep(refExpr, fieldRef)
  import RemoveStep.Target.*
  lazy val removeStep = RemoveStep(Element(refExpr), "from", refExpr)
  lazy val removeFirstStep = RemoveStep(First(Some(refExpr)), "from", refExpr)
  lazy val removeLastStep = RemoveStep(Last(None), "of", refExpr)
  lazy val pushCtxtStep = PushContextStep(x)
  import RemoveContextStep.RestoreTarget.*
  lazy val removeCtxtStep = RemoveContextStep(x, NoRestore)
  lazy val removeCtxtRestoreTopStep = RemoveContextStep(x, StackTop)
  lazy val removeCtxtRestoreStep = RemoveContextStep(x, Context(x))
  lazy val assertStep = AssertStep(compCond)
  import IfStep.ElseConfig
  lazy val ifStep = IfStep(exprCond, letStep, None)
  lazy val ifElseInlineStep =
    IfStep(exprCond, letStep, Some(letStep), ElseConfig(F, "else", T))
  lazy val ifOtherwiseInlineStep =
    IfStep(exprCond, letStep, Some(letStep), ElseConfig(F, "otherwise", T))
  lazy val ifOtherwiseInlineNoCommaStep =
    IfStep(exprCond, letStep, Some(letStep), ElseConfig(F, "otherwise", F))
  lazy val ifBlockStep =
    IfStep(exprCond, blockStep, None)
  lazy val ifElseStep =
    IfStep(exprCond, blockStep, Some(blockStep), ElseConfig())
  lazy val ifElseIfStep =
    IfStep(exprCond, blockStep, Some(ifBlockStep), ElseConfig(comma = F))
  lazy val ifElseIfElseStep =
    IfStep(exprCond, blockStep, Some(ifElseStep), ElseConfig(comma = F))
  lazy val returnStep = ReturnStep(refExpr)
  lazy val throwStep = ThrowStep("ReferenceError")
  lazy val forEachStep = ForEachStep(Some(ty), x, refExpr, T, letStep)
  lazy val forEachReverseStep =
    ForEachStep(Some(ty), x, refExpr, F, letStep)
  lazy val forEachStepNoType =
    ForEachStep(None, x, refExpr, T, letStep)
  lazy val forEachIntStep =
    ForEachIntegerStep(x, two, T, six, T, T, letStep)
  lazy val forEachIntNotIncStep =
    ForEachIntegerStep(x, two, F, six, F, T, letStep)
  lazy val forEachIntDescStep =
    ForEachIntegerStep(x, two, T, six, T, F, letStep)

  // ---------------------------------------------------------------------------
  // special steps rarely used in the spec
  // ---------------------------------------------------------------------------
  lazy val setFieldsWithIntrinsicsStep =
    SetFieldsWithIntrinsicsStep(x, "More description.")
  lazy val performBlockStep = PerformBlockStep(
    StepBlock(List(SubStep(None, letStep), SubStep(None, setStep))),
    "possibly interleaving parsing and error detection",
  )

  // ---------------------------------------------------------------------------
  // TODO refactor following code
  // ---------------------------------------------------------------------------
  lazy val forEachAscOPKStep = ForEachOwnPropertyKeyStep(
    x,
    x,
    compCond,
    T,
    ForEachOwnPropertyKeyStepOrder.NumericIndexOrder,
    letStep,
  )
  lazy val forEachDscOPKStep = ForEachOwnPropertyKeyStep(
    x,
    x,
    compCond,
    F,
    ForEachOwnPropertyKeyStepOrder.ChronologicalOrder,
    letStep,
  )
  lazy val repeatStep = RepeatStep(None, letStep)
  lazy val repeatCondStep = RepeatStep(Some(compCond), blockStep)
  lazy val noteStep = NoteStep(
    "At this point, it must be a numeric operation.",
  )
  lazy val suspendStep = SuspendStep(x, F)
  lazy val suspendAndRemoveStep = SuspendStep(x, T)
  lazy val resumeStep = ResumeEvaluationStep(x, None, None, List(subStep))
  lazy val resumeArgStep =
    ResumeEvaluationStep(x, Some(refExpr), None, List(subStep))
  lazy val resumeParamStep =
    ResumeEvaluationStep(x, None, Some(x), List(subStep))
  lazy val resumeYieldStep =
    ResumeYieldStep(x, refExpr, x, x, List(subStep))
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
  lazy val listCopyExpr = ListCopyExpression(refExpr)
  lazy val recordEmptyExpr =
    RecordExpression("Object", Nil)
  lazy val recordExpr =
    RecordExpression("Object", List(fieldLit -> refExpr))
  lazy val lengthExpr = LengthExpression(refExpr)
  lazy val substrExpr = SubstringExpression(refExpr, refExpr, None)
  lazy val substrExprTo = SubstringExpression(refExpr, refExpr, Some(refExpr))
  lazy val trim = TrimExpression(refExpr, T, T)
  lazy val trimStart = TrimExpression(refExpr, T, F)
  lazy val trimEnd = TrimExpression(refExpr, F, T)
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
  lazy val riaCheckExpr = ReturnIfAbruptExpression(invokeAOExpr, T)
  lazy val riaNoCheckExpr = ReturnIfAbruptExpression(invokeAOExpr, F)
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
  lazy val grSym = GrammarSymbolLiteral("A", Nil)
  lazy val grSymIdx = GrammarSymbolLiteral("A", List("~Yield", "+Await"))
  lazy val nt = NonterminalLiteral(None, "Identifier", Nil)
  lazy val firstNt = NonterminalLiteral(Some(1), "Identifier", Nil)
  lazy val secondNt = NonterminalLiteral(Some(2), "Identifier", Nil)
  lazy val ntFlags = NonterminalLiteral(None, "A", List("~Yield", "+Await"))
  lazy val empty = EnumLiteral("empty")
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
  lazy val typeCheckCond = TypeCheckCondition(refExpr, F, List(ty))
  lazy val notTypeCheckCond = TypeCheckCondition(refExpr, T, List(ty))
  lazy val eitherTypeCheckCond =
    TypeCheckCondition(refExpr, F, List(ty, ty, ty))
  lazy val neitherTypeCheckCond =
    TypeCheckCondition(refExpr, T, List(ty, ty))
  lazy val hasFieldCond = HasFieldCondition(x, F, fieldLit)
  lazy val noHasFieldCond = HasFieldCondition(x, T, fieldLit)
  lazy val hasBindingCond = HasBindingCondition(x, F, refExpr)
  lazy val noHasBindingCond = HasBindingCondition(x, T, refExpr)
  lazy val prodCond = ProductionCondition(nt, "Identifier", "Identifier")
  lazy val finiteCond =
    PredicateCondition(refExpr, F, PredicateConditionOperator.Finite)
  lazy val abruptCond =
    PredicateCondition(refExpr, F, PredicateConditionOperator.Abrupt)
  lazy val normalCond =
    PredicateCondition(refExpr, F, PredicateConditionOperator.Normal)
  lazy val dupCond =
    PredicateCondition(refExpr, F, PredicateConditionOperator.Duplicated)
  lazy val presentCond =
    PredicateCondition(refExpr, F, PredicateConditionOperator.Present)
  lazy val emptyCond =
    PredicateCondition(refExpr, F, PredicateConditionOperator.Empty)
  lazy val strictCond =
    PredicateCondition(refExpr, F, PredicateConditionOperator.StrictMode)
  lazy val arrayIndexCond =
    PredicateCondition(refExpr, F, PredicateConditionOperator.ArrayIndex)
  lazy val isCond = IsAreCondition(List(refExpr), F, List(lengthExpr))
  lazy val areCond =
    IsAreCondition(List(refExpr, refExpr), T, List(TrueLiteral()))
  lazy val isEitherCond =
    IsAreCondition(List(refExpr), F, List(TrueLiteral(), FalseLiteral()))
  lazy val isNeitherCond =
    IsAreCondition(List(refExpr), T, List(TrueLiteral(), FalseLiteral()))
  lazy val binaryCondLt =
    BinaryCondition(refExpr, BinaryConditionOperator.LessThan, addExpr)
  lazy val inclusiveIntervalCond =
    InclusiveIntervalCondition(
      refExpr,
      F,
      DecimalMathValueLiteral(BigDecimal(2)),
      DecimalMathValueLiteral(BigDecimal(32)),
    )
  lazy val notInclusiveIntervalCond =
    inclusiveIntervalCond.copy(negation = T)
  lazy val containsCond =
    ContainsCondition(refExpr, F, ContainsConditionTarget.Expr(refExpr))
  lazy val notContainsCond =
    ContainsCondition(refExpr, T, ContainsConditionTarget.Expr(refExpr))
  lazy val containsWhoseFieldCond = ContainsCondition(
    refExpr,
    F,
    ContainsConditionTarget.WhoseField(Some(ty), "Field", refExpr),
  )
  lazy val containsSuchThatCond = ContainsCondition(
    refExpr,
    F,
    ContainsConditionTarget.SuchThat(Some(ty), x, isCond),
  )
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
  lazy val ty = Type(RecordT("Base"))

  lazy val two = DecimalMathValueLiteral(BigDecimal(2))
  lazy val six = DecimalMathValueLiteral(BigDecimal(6))
}
