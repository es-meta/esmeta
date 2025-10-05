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

  given Encoder[Variable] =
    encoderWithType[Variable](using deriveEncoder[Variable])
  given Encoder[RunningExecutionContext] =
    encoderWithType[RunningExecutionContext](using
      deriveEncoder[RunningExecutionContext],
    )
  given Encoder[SecondExecutionContext] =
    encoderWithType[SecondExecutionContext](using
      deriveEncoder[SecondExecutionContext],
    )
  given Encoder[CurrentRealmRecord] =
    encoderWithType[CurrentRealmRecord](using deriveEncoder[CurrentRealmRecord])
  given Encoder[ActiveFunctionObject] = encoderWithType[ActiveFunctionObject](
    using deriveEncoder[ActiveFunctionObject],
  )
  given Encoder[PropertyReference] =
    encoderWithType[PropertyReference](using deriveEncoder[PropertyReference])
  given Encoder[AgentRecord] =
    encoderWithType[AgentRecord](using deriveEncoder[AgentRecord])
  given Encoder[Reference] = Encoder.instance {
    case r: Variable                => r.asJson
    case r: RunningExecutionContext => r.asJson
    case r: SecondExecutionContext  => r.asJson
    case r: CurrentRealmRecord      => r.asJson
    case r: ActiveFunctionObject    => r.asJson
    case r: PropertyReference       => r.asJson
    case r: AgentRecord             => r.asJson
  }
  given Decoder[Reference] = decoderWithParser(Reference.from)

  given Encoder[FieldProperty] =
    encoderWithType[FieldProperty](using deriveEncoder[FieldProperty])
  given Encoder[ComponentProperty] =
    encoderWithType[ComponentProperty](using deriveEncoder[ComponentProperty])
  given Encoder[BindingProperty] =
    encoderWithType[BindingProperty](using deriveEncoder[BindingProperty])
  given Encoder[IndexProperty] =
    encoderWithType[IndexProperty](using deriveEncoder[IndexProperty])
  given Encoder[IntrinsicProperty] =
    encoderWithType[IntrinsicProperty](using deriveEncoder[IntrinsicProperty])
  given Encoder[NonterminalProperty] = encoderWithType[NonterminalProperty](
    using deriveEncoder[NonterminalProperty],
  )
  given Encoder[Property] = Encoder.instance {
    case p: FieldProperty       => p.asJson
    case p: ComponentProperty   => p.asJson
    case p: BindingProperty     => p.asJson
    case p: IndexProperty       => p.asJson
    case p: IntrinsicProperty   => p.asJson
    case p: NonterminalProperty => p.asJson
  }
  given Decoder[Property] = decoderWithParser(Property.from)

  given Encoder[Intrinsic] =
    encoderWithType[Intrinsic](using deriveEncoder[Intrinsic])
  given Decoder[Intrinsic] = decoderWithParser(Intrinsic.from)

  given Encoder[StringConcatExpression] =
    encoderWithType[StringConcatExpression](using
      deriveEncoder[StringConcatExpression],
    )
  given Encoder[ListConcatExpression] = encoderWithType[ListConcatExpression](
    using deriveEncoder[ListConcatExpression],
  )
  given Encoder[ListCopyExpression] =
    encoderWithType[ListCopyExpression](using deriveEncoder[ListCopyExpression])
  given Encoder[RecordExpression] =
    encoderWithType[RecordExpression](using deriveEncoder[RecordExpression])
  given Encoder[LengthExpression] =
    encoderWithType[LengthExpression](using deriveEncoder[LengthExpression])
  given Encoder[SubstringExpression] = encoderWithType[SubstringExpression](
    using deriveEncoder[SubstringExpression],
  )
  given Encoder[TrimExpression] =
    encoderWithType[TrimExpression](using deriveEncoder[TrimExpression])
  given Encoder[NumberOfExpression] =
    encoderWithType[NumberOfExpression](using deriveEncoder[NumberOfExpression])
  given Encoder[IntrinsicExpression] = encoderWithType[IntrinsicExpression](
    using deriveEncoder[IntrinsicExpression],
  )
  given Encoder[SourceTextExpression] = encoderWithType[SourceTextExpression](
    using deriveEncoder[SourceTextExpression],
  )
  given Encoder[CoveredByExpression] = encoderWithType[CoveredByExpression](
    using deriveEncoder[CoveredByExpression],
  )
  given Encoder[GetItemsExpression] =
    encoderWithType[GetItemsExpression](using deriveEncoder[GetItemsExpression])
  given Encoder[ListExpression] =
    encoderWithType[ListExpression](using deriveEncoder[ListExpression])
  given Encoder[IntListExpression] =
    encoderWithType[IntListExpression](using deriveEncoder[IntListExpression])
  given Encoder[XRefExpression] =
    encoderWithType[XRefExpression](using deriveEncoder[XRefExpression])
  given Encoder[XRefExpressionOperator] = deriveEncoder
  given Encoder[SoleElementExpression] = encoderWithType[SoleElementExpression](
    using deriveEncoder[SoleElementExpression],
  )
  given Encoder[CodeUnitAtExpression] = encoderWithType[CodeUnitAtExpression](
    using deriveEncoder[CodeUnitAtExpression],
  )
  given Encoder[YetExpression] =
    encoderWithType[YetExpression](using deriveEncoder[YetExpression])
  given Encoder[InvokeAbstractOperationExpression] =
    encoderWithType[InvokeAbstractOperationExpression](using
      deriveEncoder[InvokeAbstractOperationExpression],
    )
  given Encoder[InvokeNumericMethodExpression] =
    encoderWithType[InvokeNumericMethodExpression](using
      deriveEncoder[InvokeNumericMethodExpression],
    )
  given Encoder[InvokeAbstractClosureExpression] =
    encoderWithType[InvokeAbstractClosureExpression](using
      deriveEncoder[InvokeAbstractClosureExpression],
    )
  given Encoder[InvokeMethodExpression] =
    encoderWithType[InvokeMethodExpression](using
      deriveEncoder[InvokeMethodExpression],
    )
  given Encoder[InvokeSyntaxDirectedOperationExpression] =
    encoderWithType[InvokeSyntaxDirectedOperationExpression](using
      deriveEncoder[InvokeSyntaxDirectedOperationExpression],
    )
  given Encoder[ReturnIfAbruptExpression] =
    encoderWithType[ReturnIfAbruptExpression](using
      deriveEncoder[ReturnIfAbruptExpression],
    )
  given Encoder[ReferenceExpression] = encoderWithType[ReferenceExpression](
    using deriveEncoder[ReferenceExpression],
  )
  given Encoder[MathFuncExpression] =
    encoderWithType[MathFuncExpression](using deriveEncoder[MathFuncExpression])
  given Encoder[MathFuncExpressionOperator] = deriveEncoder
  given Encoder[ExponentiationExpression] =
    encoderWithType[ExponentiationExpression](using
      deriveEncoder[ExponentiationExpression],
    )
  given Encoder[BinaryExpression] =
    encoderWithType[BinaryExpression](using deriveEncoder[BinaryExpression])
  given Encoder[BinaryExpressionOperator] = deriveEncoder
  given Encoder[UnaryExpression] =
    encoderWithType[UnaryExpression](using deriveEncoder[UnaryExpression])
  given Encoder[UnaryExpressionOperator] = deriveEncoder
  given Encoder[ConversionExpression] = encoderWithType[ConversionExpression](
    using deriveEncoder[ConversionExpression],
  )
  given Encoder[ConversionExpressionOperator] = deriveEncoder
  given Encoder[CalcExpression] = Encoder.instance {
    case e: ReturnIfAbruptExpression => e.asJson
    case e: ReferenceExpression      => e.asJson
    case e: MathFuncExpression       => e.asJson
    case e: ExponentiationExpression => e.asJson
    case e: BinaryExpression         => e.asJson
    case e: UnaryExpression          => e.asJson
    case e: ConversionExpression     => e.asJson
    case e: Literal                  => e.asJson
  }
  given Encoder[ClampExpression] =
    encoderWithType[ClampExpression](using deriveEncoder[ClampExpression])
  given Encoder[MathOpExpression] =
    encoderWithType[MathOpExpression](using deriveEncoder[MathOpExpression])
  given Encoder[MathOpExpressionOperator] = deriveEncoder
  given Encoder[BitwiseExpression] =
    encoderWithType[BitwiseExpression](using deriveEncoder[BitwiseExpression])
  given Encoder[BitwiseExpressionOperator] = deriveEncoder
  given Encoder[AbstractClosureExpression] =
    encoderWithType[AbstractClosureExpression](using
      deriveEncoder[AbstractClosureExpression],
    )
  given Encoder[ThisLiteral] =
    encoderWithType[ThisLiteral](using deriveEncoder[ThisLiteral])
  given Encoder[NewTargetLiteral] =
    encoderWithType[NewTargetLiteral](using deriveEncoder[NewTargetLiteral])
  given Encoder[HexLiteral] =
    encoderWithType[HexLiteral](using deriveEncoder[HexLiteral])
  given Encoder[CodeLiteral] =
    encoderWithType[CodeLiteral](using deriveEncoder[CodeLiteral])
  given Encoder[GrammarSymbolLiteral] = encoderWithType[GrammarSymbolLiteral](
    using deriveEncoder[GrammarSymbolLiteral],
  )
  given Encoder[NonterminalLiteral] =
    encoderWithType[NonterminalLiteral](using deriveEncoder[NonterminalLiteral])
  given Encoder[EnumLiteral] =
    encoderWithType[EnumLiteral](using deriveEncoder[EnumLiteral])
  given Encoder[StringLiteral] =
    encoderWithType[StringLiteral](using deriveEncoder[StringLiteral])
  given Encoder[FieldLiteral] =
    encoderWithType[FieldLiteral](using deriveEncoder[FieldLiteral])
  given Encoder[ProductionLiteral] =
    encoderWithType[ProductionLiteral](using deriveEncoder[ProductionLiteral])
  given Encoder[ErrorObjectLiteral] =
    encoderWithType[ErrorObjectLiteral](using deriveEncoder[ErrorObjectLiteral])
  given Encoder[SymbolLiteral] =
    encoderWithType[SymbolLiteral](using deriveEncoder[SymbolLiteral])
  given Encoder[PositiveInfinityMathValueLiteral] =
    encoderWithType[PositiveInfinityMathValueLiteral](using
      deriveEncoder[PositiveInfinityMathValueLiteral],
    )
  given Encoder[NegativeInfinityMathValueLiteral] =
    encoderWithType[NegativeInfinityMathValueLiteral](using
      deriveEncoder[NegativeInfinityMathValueLiteral],
    )
  given Encoder[DecimalMathValueLiteral] =
    encoderWithType[DecimalMathValueLiteral](using
      deriveEncoder[DecimalMathValueLiteral],
    )
  given Encoder[MathConstantLiteral] = encoderWithType[MathConstantLiteral](
    using deriveEncoder[MathConstantLiteral],
  )
  implicit val NumberLiteralEncoder: Encoder[NumberLiteral] =
    new Encoder[NumberLiteral] {
      final def apply(e: NumberLiteral): Json = e.double match
        case Double.PositiveInfinity =>
          Json.obj("PositiveInfinityNumberLiteral" -> Json.obj())
        case Double.NegativeInfinity =>
          Json.obj("NegativeInfinityNumberLiteral" -> Json.obj())
        case n =>
          // explicitly serialize sign information (needed for negative zero)
          val isNegative = n < 0.0 || n.equals(-0.0)
          val value = scala.math.abs(n)
          Json.obj(
            "NumberLiteral" -> Json.obj(
              "value" -> value.asJson,
              "isNegative" -> isNegative.asJson,
            ),
          )
    }
  given Encoder[BigIntLiteral] =
    encoderWithType[BigIntLiteral](using deriveEncoder[BigIntLiteral])
  given Encoder[TrueLiteral] =
    encoderWithType[TrueLiteral](using deriveEncoder[TrueLiteral])
  given Encoder[FalseLiteral] =
    encoderWithType[FalseLiteral](using deriveEncoder[FalseLiteral])
  given Encoder[UndefinedLiteral] =
    encoderWithType[UndefinedLiteral](using deriveEncoder[UndefinedLiteral])
  given Encoder[NullLiteral] =
    encoderWithType[NullLiteral](using deriveEncoder[NullLiteral])
  given Encoder[UndefinedTypeLiteral] = encoderWithType[UndefinedTypeLiteral](
    using deriveEncoder[UndefinedTypeLiteral],
  )
  given Encoder[NullTypeLiteral] =
    encoderWithType[NullTypeLiteral](using deriveEncoder[NullTypeLiteral])
  given Encoder[BooleanTypeLiteral] =
    encoderWithType[BooleanTypeLiteral](using deriveEncoder[BooleanTypeLiteral])
  given Encoder[StringTypeLiteral] =
    encoderWithType[StringTypeLiteral](using deriveEncoder[StringTypeLiteral])
  given Encoder[SymbolTypeLiteral] =
    encoderWithType[SymbolTypeLiteral](using deriveEncoder[SymbolTypeLiteral])
  given Encoder[NumberTypeLiteral] =
    encoderWithType[NumberTypeLiteral](using deriveEncoder[NumberTypeLiteral])
  given Encoder[BigIntTypeLiteral] =
    encoderWithType[BigIntTypeLiteral](using deriveEncoder[BigIntTypeLiteral])
  given Encoder[ObjectTypeLiteral] =
    encoderWithType[ObjectTypeLiteral](using deriveEncoder[ObjectTypeLiteral])
  given Encoder[Literal] = Encoder.instance {
    case lit: ThisLiteral                      => lit.asJson
    case lit: NewTargetLiteral                 => lit.asJson
    case lit: HexLiteral                       => lit.asJson
    case lit: CodeLiteral                      => lit.asJson
    case lit: GrammarSymbolLiteral             => lit.asJson
    case lit: NonterminalLiteral               => lit.asJson
    case lit: EnumLiteral                      => lit.asJson
    case lit: StringLiteral                    => lit.asJson
    case lit: FieldLiteral                     => lit.asJson
    case lit: ProductionLiteral                => lit.asJson
    case lit: ErrorObjectLiteral               => lit.asJson
    case lit: SymbolLiteral                    => lit.asJson
    case lit: PositiveInfinityMathValueLiteral => lit.asJson
    case lit: NegativeInfinityMathValueLiteral => lit.asJson
    case lit: DecimalMathValueLiteral          => lit.asJson
    case lit: MathConstantLiteral              => lit.asJson
    case lit: NumberLiteral                    => lit.asJson
    case lit: BigIntLiteral                    => lit.asJson
    case lit: TrueLiteral                      => lit.asJson
    case lit: FalseLiteral                     => lit.asJson
    case lit: UndefinedLiteral                 => lit.asJson
    case lit: NullLiteral                      => lit.asJson
    case lit: UndefinedTypeLiteral             => lit.asJson
    case lit: NullTypeLiteral                  => lit.asJson
    case lit: BooleanTypeLiteral               => lit.asJson
    case lit: StringTypeLiteral                => lit.asJson
    case lit: SymbolTypeLiteral                => lit.asJson
    case lit: NumberTypeLiteral                => lit.asJson
    case lit: BigIntTypeLiteral                => lit.asJson
    case lit: ObjectTypeLiteral                => lit.asJson
  }
  given Encoder[Expression] = Encoder.instance {
    case e: StringConcatExpression => e.asJson
    case e: ListConcatExpression   => e.asJson
    case e: ListCopyExpression     => e.asJson
    case e: RecordExpression       => e.asJson
    case e: LengthExpression       => e.asJson
    case e: SubstringExpression    => e.asJson
    case e: TrimExpression         => e.asJson
    case e: NumberOfExpression     => e.asJson
    case e: IntrinsicExpression    => e.asJson
    case e: SourceTextExpression   => e.asJson
    case e: CoveredByExpression    => e.asJson
    case e: GetItemsExpression     => e.asJson
    case e: ListExpression         => e.asJson
    case e: IntListExpression      => e.asJson
    case e: XRefExpression         => e.asJson
    case e: SoleElementExpression  => e.asJson
    case e: CodeUnitAtExpression   => e.asJson
    case e: YetExpression          => e.asJson

    case e: InvokeAbstractOperationExpression       => e.asJson
    case e: InvokeNumericMethodExpression           => e.asJson
    case e: InvokeAbstractClosureExpression         => e.asJson
    case e: InvokeMethodExpression                  => e.asJson
    case e: InvokeSyntaxDirectedOperationExpression => e.asJson

    case e: CalcExpression => e.asJson

    case e: ClampExpression   => e.asJson
    case e: MathOpExpression  => e.asJson
    case e: BitwiseExpression => e.asJson

    case e: AbstractClosureExpression => e.asJson
  }
  given Decoder[Expression] = decoderWithParser(Expression.from)
  given Decoder[CalcExpression] = decoderWithParser(CalcExpression.from)
  given Decoder[BitwiseExpression] = decoderWithParser(BitwiseExpression.from)
  given Decoder[ClampExpression] = decoderWithParser(ClampExpression.from)
  given Decoder[MathOpExpression] = decoderWithParser(MathOpExpression.from)
  given Decoder[Literal] = decoderWithParser(Literal.from)

  given Encoder[ExpressionCondition] =
    encoderWithType[ExpressionCondition](using
      deriveEncoder[ExpressionCondition],
    )
  given Encoder[TypeCheckCondition] =
    encoderWithType[TypeCheckCondition](using deriveEncoder[TypeCheckCondition])
  given Encoder[HasFieldCondition] =
    encoderWithType[HasFieldCondition](using deriveEncoder[HasFieldCondition])
  given Encoder[HasBindingCondition] =
    encoderWithType[HasBindingCondition](using
      deriveEncoder[HasBindingCondition],
    )
  given Encoder[ProductionCondition] =
    encoderWithType[ProductionCondition](using
      deriveEncoder[ProductionCondition],
    )
  given Encoder[PredicateCondition] =
    encoderWithType[PredicateCondition](using deriveEncoder[PredicateCondition])
  given Encoder[PredicateConditionOperator] = deriveEncoder
  given Encoder[IsAreCondition] =
    encoderWithType[IsAreCondition](using deriveEncoder[IsAreCondition])
  given Encoder[BinaryCondition] =
    encoderWithType[BinaryCondition](using deriveEncoder[BinaryCondition])
  given Encoder[BinaryConditionOperator] = deriveEncoder
  given Encoder[InclusiveIntervalCondition] =
    encoderWithType[InclusiveIntervalCondition](using
      deriveEncoder[InclusiveIntervalCondition],
    )
  given Encoder[ContainsCondition] =
    encoderWithType[ContainsCondition](using deriveEncoder[ContainsCondition])
  given Encoder[ContainsConditionTarget] = deriveEncoder
  given Encoder[CompoundCondition] =
    encoderWithType[CompoundCondition](using deriveEncoder[CompoundCondition])
  given Encoder[CompoundConditionOperator] = deriveEncoder
  given Encoder[Condition] = Encoder.instance {
    case c: ExpressionCondition        => c.asJson
    case c: TypeCheckCondition         => c.asJson
    case c: HasFieldCondition          => c.asJson
    case c: HasBindingCondition        => c.asJson
    case c: ProductionCondition        => c.asJson
    case c: PredicateCondition         => c.asJson
    case c: IsAreCondition             => c.asJson
    case c: BinaryCondition            => c.asJson
    case c: InclusiveIntervalCondition => c.asJson
    case c: ContainsCondition          => c.asJson
    case c: CompoundCondition          => c.asJson
  }
  given Decoder[Condition] = decoderWithParser(Condition.from)

  given Encoder[Type] = encoderWithStringifier(stringify)
  given Decoder[Type] = decoderWithParser(Type.from)

  given Encoder[StepBlock] =
    encoderWithType[StepBlock](using deriveEncoder[StepBlock])
  given Encoder[ExprBlock] =
    encoderWithType[ExprBlock](using deriveEncoder[ExprBlock])
  given Encoder[Figure] = encoderWithType[Figure](using deriveEncoder[Figure])
  given Encoder[SubStep] =
    encoderWithType[SubStep](using deriveEncoder[SubStep])
  given Encoder[Directive] =
    encoderWithType[Directive](using deriveEncoder[Directive])
  given Encoder[Block] = Encoder.instance {
    case b: StepBlock => b.asJson
    case b: ExprBlock => b.asJson
    case b: Figure    => b.asJson
  }
  given Decoder[Block] = decoderWithParser(Block.from)

  given Encoder[LetStep] =
    encoderWithType[LetStep](using deriveEncoder[LetStep])
  given Encoder[SetStep] =
    encoderWithType[SetStep](using deriveEncoder[SetStep])
  given Encoder[SetAsStep] =
    encoderWithType[SetAsStep](using deriveEncoder[SetAsStep])
  given Encoder[InvokeShorthandStep] = encoderWithType[InvokeShorthandStep](
    using deriveEncoder[InvokeShorthandStep],
  )
  given Encoder[IfStep] = encoderWithType[IfStep](using deriveEncoder[IfStep])
  given Encoder[IfStep.ElseConfig] =
    encoderWithType[IfStep.ElseConfig](using deriveEncoder[IfStep.ElseConfig])
  given Encoder[ReturnStep] =
    encoderWithType[ReturnStep](using deriveEncoder[ReturnStep])
  given Encoder[AssertStep] =
    encoderWithType[AssertStep](using deriveEncoder[AssertStep])
  given Encoder[ForEachStep] =
    encoderWithType[ForEachStep](using deriveEncoder[ForEachStep])
  given Encoder[ForEachIntegerStep] =
    encoderWithType[ForEachIntegerStep](using deriveEncoder[ForEachIntegerStep])
  given Encoder[ForEachOwnPropertyKeyStep] =
    encoderWithType[ForEachOwnPropertyKeyStep](using
      deriveEncoder[ForEachOwnPropertyKeyStep],
    )
  given Encoder[ForEachOwnPropertyKeyStepOrder] = deriveEncoder
  given Encoder[ForEachParseNodeStep] = encoderWithType[ForEachParseNodeStep](
    using deriveEncoder[ForEachParseNodeStep],
  )
  given Encoder[ThrowStep] =
    encoderWithType[ThrowStep](using deriveEncoder[ThrowStep])
  given Encoder[PerformStep] =
    encoderWithType[PerformStep](using deriveEncoder[PerformStep])
  given Encoder[PerformBlockStep] =
    encoderWithType[PerformBlockStep](using deriveEncoder[PerformBlockStep])
  given Encoder[AppendStep] =
    encoderWithType[AppendStep](using deriveEncoder[AppendStep])
  given Encoder[PrependStep] =
    encoderWithType[PrependStep](using deriveEncoder[PrependStep])
  given Encoder[AddStep] =
    encoderWithType[AddStep](using deriveEncoder[AddStep])
  given Encoder[RepeatStep] =
    encoderWithType[RepeatStep](using deriveEncoder[RepeatStep])
  given Encoder[RepeatStep.LoopCondition] = deriveEncoder
  given Encoder[PushContextStep] =
    encoderWithType[PushContextStep](using deriveEncoder[PushContextStep])
  given Encoder[NoteStep] =
    encoderWithType[NoteStep](using deriveEncoder[NoteStep])
  given Encoder[SuspendStep] =
    encoderWithType[SuspendStep](using deriveEncoder[SuspendStep])
  given Encoder[ResumeStep] =
    encoderWithType[ResumeStep](using deriveEncoder[ResumeStep])
  given Encoder[ResumeTopContextStep] = encoderWithType[ResumeTopContextStep](
    using deriveEncoder[ResumeTopContextStep],
  )
  given Encoder[RemoveStep] =
    encoderWithType[RemoveStep](using deriveEncoder[RemoveStep])
  given Encoder[RemoveStep.Target] = deriveEncoder
  given Encoder[RemoveContextStep] =
    encoderWithType[RemoveContextStep](using deriveEncoder[RemoveContextStep])
  given Encoder[RemoveContextStep.RestoreTarget] = deriveEncoder
  given Encoder[SetEvaluationStateStep] =
    encoderWithType[SetEvaluationStateStep](using
      deriveEncoder[SetEvaluationStateStep],
    )
  given Encoder[ResumeEvaluationStep] = encoderWithType[ResumeEvaluationStep](
    using deriveEncoder[ResumeEvaluationStep],
  )
  given Encoder[SetFieldsWithIntrinsicsStep] =
    encoderWithType[SetFieldsWithIntrinsicsStep](using
      deriveEncoder[SetFieldsWithIntrinsicsStep],
    )
  given Encoder[BlockStep] =
    encoderWithType[BlockStep](using deriveEncoder[BlockStep])
  given Encoder[YetStep] =
    encoderWithType[YetStep](using deriveEncoder[YetStep])
  given Encoder[Step] = Encoder.instance {
    case step: LetStep                     => step.asJson
    case step: SetStep                     => step.asJson
    case step: SetAsStep                   => step.asJson
    case step: IfStep                      => step.asJson
    case step: InvokeShorthandStep         => step.asJson
    case step: ReturnStep                  => step.asJson
    case step: AssertStep                  => step.asJson
    case step: ForEachStep                 => step.asJson
    case step: ForEachIntegerStep          => step.asJson
    case step: ForEachOwnPropertyKeyStep   => step.asJson
    case step: ForEachParseNodeStep        => step.asJson
    case step: ThrowStep                   => step.asJson
    case step: PerformStep                 => step.asJson
    case step: PerformBlockStep            => step.asJson
    case step: AppendStep                  => step.asJson
    case step: PrependStep                 => step.asJson
    case step: AddStep                     => step.asJson
    case step: RepeatStep                  => step.asJson
    case step: PushContextStep             => step.asJson
    case step: NoteStep                    => step.asJson
    case step: SuspendStep                 => step.asJson
    case step: ResumeStep                  => step.asJson
    case step: ResumeTopContextStep        => step.asJson
    case step: RemoveStep                  => step.asJson
    case step: RemoveContextStep           => step.asJson
    case step: SetEvaluationStateStep      => step.asJson
    case step: ResumeEvaluationStep        => step.asJson
    case step: SetFieldsWithIntrinsicsStep => step.asJson
    case step: BlockStep                   => step.asJson
    case step: YetStep                     => step.asJson
  }
  given Decoder[Step] = decoderWithParser(Step.from)

  given Decoder[Directive] = deriveDecoder
}
