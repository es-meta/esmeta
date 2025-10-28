package esmeta
package lang
package util

import esmeta.util.*
import esmeta.util.BaseUtils.*
import io.circe.*, io.circe.syntax.*, io.circe.generic.semiauto.*
import esmeta.error.ESMetaError

object JsonProtocol extends BasicJsonProtocol {
  val stringifier = LangElem.getStringifier(true, false)
  import stringifier.given

  // references
  given Decoder[Reference] = deriveDecoderWithType
  given Encoder[Reference] = deriveEncoderWithType
  given Decoder[Variable] = deriveDecoderWithType
  given Encoder[Variable] = deriveEncoderWithType
  given Decoder[RunningExecutionContext] = deriveDecoderWithType
  given Encoder[RunningExecutionContext] = deriveEncoderWithType
  given Decoder[SecondExecutionContext] = deriveDecoderWithType
  given Encoder[SecondExecutionContext] = deriveEncoderWithType
  given Decoder[CurrentRealmRecord] = deriveDecoderWithType
  given Encoder[CurrentRealmRecord] = deriveEncoderWithType
  given Decoder[ActiveFunctionObject] = deriveDecoderWithType
  given Encoder[ActiveFunctionObject] = deriveEncoderWithType
  given Decoder[PropertyReference] = deriveDecoderWithType
  given Encoder[PropertyReference] = deriveEncoderWithType
  given Decoder[AgentRecord] = deriveDecoderWithType
  given Encoder[AgentRecord] = deriveEncoderWithType

  // properties
  given Decoder[Property] = deriveDecoderWithType
  given Encoder[Property] = deriveEncoderWithType
  given Decoder[FieldProperty] = deriveDecoderWithType
  given Encoder[FieldProperty] = deriveEncoderWithType
  given Decoder[FieldPropertyForm] = deriveDecoderWithType
  given Encoder[FieldPropertyForm] = deriveEncoderWithType
  given Decoder[ComponentProperty] = deriveDecoderWithType
  given Encoder[ComponentProperty] = deriveEncoderWithType
  given Decoder[ComponentPropertyForm] = deriveDecoderWithType
  given Encoder[ComponentPropertyForm] = deriveEncoderWithType
  given Decoder[BindingProperty] = deriveDecoderWithType
  given Encoder[BindingProperty] = deriveEncoderWithType
  given Decoder[IndexProperty] = deriveDecoderWithType
  given Encoder[IndexProperty] = deriveEncoderWithType
  given Decoder[PositionalElementProperty] = deriveDecoderWithType
  given Encoder[PositionalElementProperty] = deriveEncoderWithType
  given Decoder[IntrinsicProperty] = deriveDecoderWithType
  given Encoder[IntrinsicProperty] = deriveEncoderWithType
  given Decoder[NonterminalProperty] = deriveDecoderWithType
  given Encoder[NonterminalProperty] = deriveEncoderWithType

  // intrinsics
  given Decoder[Intrinsic] = deriveDecoderWithType
  given Encoder[Intrinsic] = deriveEncoderWithType

  // expressions
  given Decoder[Expression] = deriveDecoderWithType
  given Encoder[Expression] = deriveEncoderWithType
  given Decoder[StringConcatExpression] = deriveDecoderWithType
  given Encoder[StringConcatExpression] = deriveEncoderWithType
  given Decoder[ListConcatExpression] = deriveDecoderWithType
  given Encoder[ListConcatExpression] = deriveEncoderWithType
  given Decoder[ListCopyExpression] = deriveDecoderWithType
  given Encoder[ListCopyExpression] = deriveEncoderWithType
  given Decoder[RecordExpression] = deriveDecoderWithType
  given Encoder[RecordExpression] = deriveEncoderWithType
  given Decoder[RecordExpressionForm] = deriveDecoderWithType
  given Encoder[RecordExpressionForm] = deriveEncoderWithType
  given Decoder[LengthExpression] = deriveDecoderWithType
  given Encoder[LengthExpression] = deriveEncoderWithType
  given Decoder[SubstringExpression] = deriveDecoderWithType
  given Encoder[SubstringExpression] = deriveEncoderWithType
  given Decoder[TrimExpression] = deriveDecoderWithType
  given Encoder[TrimExpression] = deriveEncoderWithType
  given Decoder[NumberOfExpression] = deriveDecoderWithType
  given Encoder[NumberOfExpression] = deriveEncoderWithType
  given Decoder[IntrinsicExpression] = deriveDecoderWithType
  given Encoder[IntrinsicExpression] = deriveEncoderWithType
  given Decoder[SourceTextExpression] = deriveDecoderWithType
  given Encoder[SourceTextExpression] = deriveEncoderWithType
  given Decoder[CoveredByExpression] = deriveDecoderWithType
  given Encoder[CoveredByExpression] = deriveEncoderWithType
  given Decoder[GetItemsExpression] = deriveDecoderWithType
  given Encoder[GetItemsExpression] = deriveEncoderWithType
  given Decoder[ListExpression] = deriveDecoderWithType
  given Encoder[ListExpression] = deriveEncoderWithType
  given Decoder[ListExpressionForm] = deriveDecoderWithType
  given Encoder[ListExpressionForm] = deriveEncoderWithType
  given Decoder[XRefExpression] = deriveDecoderWithType
  given Encoder[XRefExpression] = deriveEncoderWithType
  given Decoder[XRefExpressionOperator] = deriveDecoderWithType
  given Encoder[XRefExpressionOperator] = deriveEncoder
  given Decoder[SoleElementExpression] = deriveDecoderWithType
  given Encoder[SoleElementExpression] = deriveEncoderWithType
  given Decoder[CodeUnitAtExpression] = deriveDecoderWithType
  given Encoder[CodeUnitAtExpression] = deriveEncoderWithType
  given Decoder[YetExpression] = deriveDecoderWithType
  given Encoder[YetExpression] = deriveEncoderWithType

  // invocation expressions
  given Decoder[InvokeExpression] = deriveDecoderWithType
  given Encoder[InvokeExpression] = deriveEncoderWithType
  given Decoder[HtmlTag] = deriveDecoderWithType
  given Encoder[HtmlTag] = deriveEncoderWithType
  given Decoder[InvokeAbstractOperationExpression] = deriveDecoderWithType
  given Encoder[InvokeAbstractOperationExpression] = deriveEncoderWithType
  given Decoder[InvokeNumericMethodExpression] = deriveDecoderWithType
  given Encoder[InvokeNumericMethodExpression] = deriveEncoderWithType
  given Decoder[InvokeAbstractClosureExpression] = deriveDecoderWithType
  given Encoder[InvokeAbstractClosureExpression] = deriveEncoderWithType
  given Decoder[InvokeMethodExpression] = deriveDecoderWithType
  given Encoder[InvokeMethodExpression] = deriveEncoderWithType
  given Decoder[InvokeSyntaxDirectedOperationExpression] = deriveDecoderWithType
  given Encoder[InvokeSyntaxDirectedOperationExpression] = deriveEncoderWithType

  // calculation expressions
  given Decoder[CalcExpression] = deriveDecoderWithType
  given Encoder[CalcExpression] = deriveEncoderWithType
  given Decoder[ReturnIfAbruptExpression] = deriveDecoderWithType
  given Encoder[ReturnIfAbruptExpression] = deriveEncoderWithType
  given Decoder[ReferenceExpression] = deriveDecoderWithType
  given Encoder[ReferenceExpression] = deriveEncoderWithType
  given Decoder[MathFuncExpression] = deriveDecoderWithType
  given Encoder[MathFuncExpression] = deriveEncoderWithType
  given Decoder[MathFuncExpressionOperator] = deriveDecoderWithType
  given Encoder[MathFuncExpressionOperator] = deriveEncoderWithType
  given Decoder[ExponentiationExpression] = deriveDecoderWithType
  given Encoder[ExponentiationExpression] = deriveEncoderWithType
  given Decoder[BinaryExpression] = deriveDecoderWithType
  given Encoder[BinaryExpression] = deriveEncoderWithType
  given Decoder[BinaryExpressionOperator] = deriveDecoderWithType
  given Encoder[BinaryExpressionOperator] = deriveEncoderWithType
  given Decoder[UnaryExpression] = deriveDecoderWithType
  given Encoder[UnaryExpression] = deriveEncoderWithType
  given Decoder[UnaryExpressionOperator] = deriveDecoderWithType
  given Encoder[UnaryExpressionOperator] = deriveEncoderWithType
  given Decoder[ConversionExpression] = deriveDecoderWithType
  given Encoder[ConversionExpression] = deriveEncoderWithType
  given Decoder[ConversionExpressionOperator] = deriveDecoderWithType
  given Encoder[ConversionExpressionOperator] = deriveEncoderWithType
  given Decoder[ConversionExpressionForm] = deriveDecoderWithType
  given Encoder[ConversionExpressionForm] = deriveEncoderWithType

  // clamp expressions
  given Decoder[ClampExpression] = deriveDecoderWithType
  given Encoder[ClampExpression] = deriveEncoderWithType

  // mathematical operation expressions
  given Decoder[MathOpExpression] = deriveDecoderWithType
  given Encoder[MathOpExpression] = deriveEncoderWithType
  given Decoder[MathOpExpressionOperator] = deriveDecoderWithType
  given Encoder[MathOpExpressionOperator] = deriveEncoderWithType

  // bitwise expressions
  given Decoder[BitwiseExpression] = deriveDecoderWithType
  given Encoder[BitwiseExpression] = deriveEncoderWithType
  given Decoder[BitwiseExpressionOperator] = deriveDecoderWithType
  given Encoder[BitwiseExpressionOperator] = deriveEncoderWithType

  // multiline expressions
  given Decoder[MultilineExpression] = deriveDecoderWithType
  given Encoder[MultilineExpression] = deriveEncoderWithType
  given Decoder[AbstractClosureExpression] = deriveDecoderWithType
  given Encoder[AbstractClosureExpression] = deriveEncoderWithType

  // literals
  given Decoder[Literal] = deriveDecoderWithType
  given Encoder[Literal] = deriveEncoderWithType
  given Decoder[ThisLiteral] = deriveDecoderWithType
  given Encoder[ThisLiteral] = deriveEncoderWithType
  given Decoder[NewTargetLiteral] = deriveDecoderWithType
  given Encoder[NewTargetLiteral] = deriveEncoderWithType
  given Decoder[HexLiteral] = deriveDecoderWithType
  given Encoder[HexLiteral] = deriveEncoderWithType
  given Decoder[CodeLiteral] = deriveDecoderWithType
  given Encoder[CodeLiteral] = deriveEncoderWithType
  given Decoder[GrammarSymbolLiteral] = deriveDecoderWithType
  given Encoder[GrammarSymbolLiteral] = deriveEncoderWithType
  given Decoder[NonterminalLiteral] = deriveDecoderWithType
  given Encoder[NonterminalLiteral] = deriveEncoderWithType
  given Decoder[EnumLiteral] = deriveDecoderWithType
  given Encoder[EnumLiteral] = deriveEncoderWithType
  given Decoder[StringLiteral] = deriveDecoderWithType
  given Encoder[StringLiteral] = deriveEncoderWithType
  given Decoder[StringLiteralForm] = deriveDecoderWithType
  given Encoder[StringLiteralForm] = deriveEncoderWithType
  given Decoder[FieldLiteral] = deriveDecoderWithType
  given Encoder[FieldLiteral] = deriveEncoderWithType
  given Decoder[ProductionLiteral] = deriveDecoderWithType
  given Encoder[ProductionLiteral] = deriveEncoderWithType
  given Decoder[ErrorObjectLiteral] = deriveDecoderWithType
  given Encoder[ErrorObjectLiteral] = deriveEncoderWithType
  given Decoder[SymbolLiteral] = deriveDecoderWithType
  given Encoder[SymbolLiteral] = deriveEncoderWithType
  given Decoder[PositiveInfinityMathValueLiteral] = deriveDecoderWithType
  given Encoder[PositiveInfinityMathValueLiteral] = deriveEncoderWithType
  given Decoder[NegativeInfinityMathValueLiteral] = deriveDecoderWithType
  given Encoder[NegativeInfinityMathValueLiteral] = deriveEncoderWithType
  given Decoder[DecimalMathValueLiteral] = deriveDecoderWithType
  given Encoder[DecimalMathValueLiteral] = deriveEncoderWithType
  given Decoder[MathConstantLiteral] = deriveDecoderWithType
  given Encoder[MathConstantLiteral] = deriveEncoderWithType
  given Decoder[NumberLiteral] = deriveDecoderWithType
  given Encoder[NumberLiteral] = deriveEncoderWithType
  given Decoder[BigIntLiteral] = deriveDecoderWithType
  given Encoder[BigIntLiteral] = deriveEncoderWithType
  given Decoder[TrueLiteral] = deriveDecoderWithType
  given Encoder[TrueLiteral] = deriveEncoderWithType
  given Decoder[FalseLiteral] = deriveDecoderWithType
  given Encoder[FalseLiteral] = deriveEncoderWithType
  given Decoder[UndefinedLiteral] = deriveDecoderWithType
  given Encoder[UndefinedLiteral] = deriveEncoderWithType
  given Decoder[NullLiteral] = deriveDecoderWithType
  given Encoder[NullLiteral] = deriveEncoderWithType
  given Decoder[UndefinedTypeLiteral] = deriveDecoderWithType
  given Encoder[UndefinedTypeLiteral] = deriveEncoderWithType
  given Decoder[NullTypeLiteral] = deriveDecoderWithType
  given Encoder[NullTypeLiteral] = deriveEncoderWithType
  given Decoder[BooleanTypeLiteral] = deriveDecoderWithType
  given Encoder[BooleanTypeLiteral] = deriveEncoderWithType
  given Decoder[StringTypeLiteral] = deriveDecoderWithType
  given Encoder[StringTypeLiteral] = deriveEncoderWithType
  given Decoder[SymbolTypeLiteral] = deriveDecoderWithType
  given Encoder[SymbolTypeLiteral] = deriveEncoderWithType
  given Decoder[NumberTypeLiteral] = deriveDecoderWithType
  given Encoder[NumberTypeLiteral] = deriveEncoderWithType
  given Decoder[BigIntTypeLiteral] = deriveDecoderWithType
  given Encoder[BigIntTypeLiteral] = deriveEncoderWithType
  given Decoder[ObjectTypeLiteral] = deriveDecoderWithType
  given Encoder[ObjectTypeLiteral] = deriveEncoderWithType

  // conditions
  given Decoder[Condition] = deriveDecoderWithType
  given Encoder[Condition] = deriveEncoderWithType
  given Decoder[ExpressionCondition] = deriveDecoderWithType
  given Encoder[ExpressionCondition] = deriveEncoderWithType
  given Decoder[TypeCheckCondition] = deriveDecoderWithType
  given Encoder[TypeCheckCondition] = deriveEncoderWithType
  given Decoder[HasFieldCondition] = deriveDecoderWithType
  given Encoder[HasFieldCondition] = deriveEncoderWithType
  given Decoder[HasFieldConditionForm] = deriveDecoderWithType
  given Encoder[HasFieldConditionForm] = deriveEncoderWithType
  given Decoder[HasBindingCondition] = deriveDecoderWithType
  given Encoder[HasBindingCondition] = deriveEncoderWithType
  given Decoder[ProductionCondition] = deriveDecoderWithType
  given Encoder[ProductionCondition] = deriveEncoderWithType
  given Decoder[PredicateCondition] = deriveDecoderWithType
  given Encoder[PredicateCondition] = deriveEncoderWithType
  given Decoder[PredicateConditionOperator] = deriveDecoderWithType
  given Encoder[PredicateConditionOperator] = deriveEncoderWithType
  given Decoder[IsAreCondition] = deriveDecoderWithType
  given Encoder[IsAreCondition] = deriveEncoderWithType
  given Decoder[BinaryCondition] = deriveDecoderWithType
  given Encoder[BinaryCondition] = deriveEncoderWithType
  given Decoder[BinaryConditionOperator] = deriveDecoderWithType
  given Encoder[BinaryConditionOperator] = deriveEncoderWithType
  given Decoder[InclusiveIntervalCondition] = deriveDecoderWithType
  given Encoder[InclusiveIntervalCondition] = deriveEncoderWithType
  given Decoder[ContainsCondition] = deriveDecoderWithType
  given Encoder[ContainsCondition] = deriveEncoderWithType
  given Decoder[ContainsConditionTarget] = deriveDecoderWithType
  given Encoder[ContainsConditionTarget] = deriveEncoderWithType
  given Decoder[CompoundCondition] = deriveDecoderWithType
  given Encoder[CompoundCondition] = deriveEncoderWithType
  given Decoder[CompoundConditionOperator] = deriveDecoderWithType
  given Encoder[CompoundConditionOperator] = deriveEncoderWithType

  // types
  given Decoder[Type] = decoderWithParser(Type.from)
  given Encoder[Type] = encoderWithStringifier(stringify)

  // steps
  given Decoder[LetStep] = deriveDecoderWithType
  given Encoder[LetStep] = deriveEncoderWithType
  given Decoder[SetStep] = deriveDecoderWithType
  given Encoder[SetStep] = deriveEncoderWithType
  given Decoder[SetAsStep] = deriveDecoderWithType
  given Encoder[SetAsStep] = deriveEncoderWithType
  given Decoder[InvokeShorthandStep] = deriveDecoderWithType
  given Encoder[InvokeShorthandStep] = deriveEncoderWithType
  given Decoder[IfStep] = deriveDecoderWithType
  given Encoder[IfStep] = deriveEncoderWithType
  given Decoder[IfStep.ElseConfig] = deriveDecoderWithType
  given Encoder[IfStep.ElseConfig] = deriveEncoderWithType
  given Decoder[ReturnStep] = deriveDecoderWithType
  given Encoder[ReturnStep] = deriveEncoderWithType
  given Decoder[AssertStep] = deriveDecoderWithType
  given Encoder[AssertStep] = deriveEncoderWithType
  given Decoder[ForEachStep] = deriveDecoderWithType
  given Encoder[ForEachStep] = deriveEncoderWithType
  given Decoder[ForEachIntegerStep] = deriveDecoderWithType
  given Encoder[ForEachIntegerStep] = deriveEncoderWithType
  given Decoder[ForEachOwnPropertyKeyStep] = deriveDecoderWithType
  given Encoder[ForEachOwnPropertyKeyStep] = deriveEncoderWithType
  given Decoder[ForEachOwnPropertyKeyStepOrder] = deriveDecoderWithType
  given Encoder[ForEachOwnPropertyKeyStepOrder] = deriveEncoderWithType
  given Decoder[ForEachParseNodeStep] = deriveDecoderWithType
  given Encoder[ForEachParseNodeStep] = deriveEncoderWithType
  given Decoder[ThrowStep] = deriveDecoderWithType
  given Encoder[ThrowStep] = deriveEncoderWithType
  given Decoder[PerformStep] = deriveDecoderWithType
  given Encoder[PerformStep] = deriveEncoderWithType
  given Decoder[PerformBlockStep] = deriveDecoderWithType
  given Encoder[PerformBlockStep] = deriveEncoderWithType
  given Decoder[AppendStep] = deriveDecoderWithType
  given Encoder[AppendStep] = deriveEncoderWithType
  given Decoder[PrependStep] = deriveDecoderWithType
  given Encoder[PrependStep] = deriveEncoderWithType
  given Decoder[AddStep] = deriveDecoderWithType
  given Encoder[AddStep] = deriveEncoderWithType
  given Decoder[RepeatStep] = deriveDecoderWithType
  given Encoder[RepeatStep] = deriveEncoderWithType
  given Decoder[RepeatStep.LoopCondition] = deriveDecoderWithType
  given Encoder[RepeatStep.LoopCondition] = deriveEncoderWithType
  given Decoder[PushContextStep] = deriveDecoderWithType
  given Encoder[PushContextStep] = deriveEncoderWithType
  given Decoder[NoteStep] = deriveDecoderWithType
  given Encoder[NoteStep] = deriveEncoderWithType
  given Decoder[SuspendStep] = deriveDecoderWithType
  given Encoder[SuspendStep] = deriveEncoderWithType
  given Decoder[ResumeStep] = deriveDecoderWithType
  given Encoder[ResumeStep] = deriveEncoderWithType
  given Decoder[ResumeTopContextStep] = deriveDecoderWithType
  given Encoder[ResumeTopContextStep] = deriveEncoderWithType
  given Decoder[RemoveStep] = deriveDecoderWithType
  given Encoder[RemoveStep] = deriveEncoderWithType
  given Decoder[RemoveStep.Target] = deriveDecoderWithType
  given Encoder[RemoveStep.Target] = deriveEncoderWithType
  given Decoder[RemoveContextStep] = deriveDecoderWithType
  given Encoder[RemoveContextStep] = deriveEncoderWithType
  given Decoder[RemoveContextStep.RestoreTarget] = deriveDecoderWithType
  given Encoder[RemoveContextStep.RestoreTarget] = deriveEncoderWithType
  given Decoder[SetEvaluationStateStep] = deriveDecoderWithType
  given Encoder[SetEvaluationStateStep] = deriveEncoderWithType
  given Decoder[ResumeEvaluationStep] = deriveDecoderWithType
  given Encoder[ResumeEvaluationStep] = deriveEncoderWithType
  given Decoder[SetFieldsWithIntrinsicsStep] = deriveDecoderWithType
  given Encoder[SetFieldsWithIntrinsicsStep] = deriveEncoderWithType
  given Decoder[BlockStep] = deriveDecoderWithType
  given Encoder[BlockStep] = deriveEncoderWithType
  given Decoder[YetStep] = deriveDecoderWithType
  given Encoder[YetStep] = deriveEncoderWithType
  given Decoder[Step] = deriveDecoderWithType
  given Encoder[Step] = deriveEncoderWithType

  // others
  given Decoder[StepBlock] = deriveDecoderWithType
  given Encoder[StepBlock] = deriveEncoderWithType
  given Decoder[ExprBlock] = deriveDecoderWithType
  given Encoder[ExprBlock] = deriveEncoderWithType
  given Decoder[Figure] = deriveDecoderWithType
  given Encoder[Figure] = deriveEncoderWithType
  given Decoder[SubStep] = deriveDecoderWithType
  given Encoder[SubStep] = deriveEncoderWithType
  given Decoder[Directive] = deriveDecoderWithType
  given Encoder[Directive] = deriveEncoderWithType
  given Decoder[Block] = deriveDecoderWithType
  given Encoder[Block] = deriveEncoderWithType

  // basics
  given Decoder[Double] = doubleDecoder
  given Encoder[Double] = doubleEncoder
}
