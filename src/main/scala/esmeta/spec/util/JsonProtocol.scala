package esmeta
package spec
package util

import esmeta.lang.util.JsonProtocol.given
import esmeta.ty.*
import esmeta.ty.util.JsonProtocol.given
import esmeta.util.*
import esmeta.util.BaseUtils.*
import io.circe.*, io.circe.syntax.*, io.circe.generic.semiauto.*

object JsonProtocol extends BasicJsonProtocol {
  // ECMAScript specifications (ECMA-262)
  given Decoder[Spec] = deriveDecoder
  given Encoder[Spec] = deriveEncoder

  // version
  given Decoder[Spec.Version] = deriveDecoder
  given Encoder[Spec.Version] = deriveEncoder

  // tables
  given Decoder[Table] = deriveDecoder
  given Encoder[Table] = deriveEncoder

  // ---------------------------------------------------------------------------
  // Grammar
  // ---------------------------------------------------------------------------
  given Decoder[Grammar] = deriveDecoder
  given Encoder[Grammar] = deriveEncoder
  // productions for ECMAScript grammars
  given Decoder[Production] = deriveDecoder
  given Encoder[Production] = deriveEncoder
  given Decoder[ProductionKind] = deriveDecoder
  given Encoder[ProductionKind] = deriveEncoder
  // left-hand-sides (LHSs) of productions
  given Decoder[Lhs] = deriveDecoder
  given Encoder[Lhs] = deriveEncoder
  // alternatives or right-hand-sides (RHSs) of productions */
  given Decoder[Rhs] = deriveDecoder
  given Encoder[Rhs] = deriveEncoder
  // alternatives or right-hand-sides (RHSs) of productions */
  given Decoder[RhsCond] = deriveDecoder
  given Encoder[RhsCond] = deriveEncoder
  // symbols
  given Decoder[Symbol] =
    decoderWithDiscriminator("symbol", symbolDiscriminators)
  private lazy val symbolDiscriminators
    : List[(String, HCursor => Decoder.Result[Symbol])] = List(
    "term" -> (_.as[Terminal]),
    "name" -> (_.as[Nonterminal]),
    "symbol" -> (_.as[Optional]),
    "notCases" -> (_.as[ButNot]),
    "methodName" -> (_.as[ButOnlyIf]),
    "cases" -> (_.as[Lookahead]),
    "empty" -> (_ => Right(Empty)),
    "nlt" -> (_ => Right(NoLineTerminator)),
    "cp" -> (_.as[CodePoint]),
    "abbr" -> (_.as[CodePointAbbr]),
    "cpCond" -> (_.as[UnicodeSet]),
  )
  given Encoder[Symbol] = Encoder.instance {
    case symbol: Terminal      => symbol.asJson
    case symbol: Nonterminal   => symbol.asJson
    case symbol: Optional      => symbol.asJson
    case symbol: ButNot        => symbol.asJson
    case symbol: ButOnlyIf     => symbol.asJson
    case symbol: Lookahead     => symbol.asJson
    case Empty                 => Json.obj("empty" -> Json.Null)
    case NoLineTerminator      => Json.obj("nlt" -> Json.Null)
    case symbol: CodePoint     => symbol.asJson
    case symbol: CodePointAbbr => symbol.asJson
    case symbol: UnicodeSet    => symbol.asJson
  }
  given Decoder[Terminal] = deriveDecoder
  given Encoder[Terminal] = deriveEncoder
  given Decoder[Nonterminal] = deriveDecoder
  given Encoder[Nonterminal] = deriveEncoder
  given Decoder[NonterminalArgument] = deriveDecoder
  given Encoder[NonterminalArgument] = deriveEncoder
  given Decoder[NonterminalArgumentKind] = deriveDecoder
  given Encoder[NonterminalArgumentKind] = deriveEncoder
  given Decoder[Optional] = deriveDecoder
  given Encoder[Optional] = deriveEncoder
  given Decoder[ButNot] = deriveDecoder
  given Encoder[ButNot] = deriveEncoder
  given Decoder[ButOnlyIf] = deriveDecoder
  given Encoder[ButOnlyIf] = deriveEncoder
  given Decoder[Lookahead] = deriveDecoder
  given Encoder[Lookahead] = deriveEncoder
  given Decoder[CodePoint] = deriveDecoder
  given Encoder[CodePoint] = deriveEncoder
  given Decoder[CodePointAbbr] = deriveDecoder
  given Encoder[CodePointAbbr] = deriveEncoder
  given Decoder[UnicodeSet] = deriveDecoder
  given Encoder[UnicodeSet] = deriveEncoder

  // ---------------------------------------------------------------------------
  // Algorithms
  // ---------------------------------------------------------------------------
  given Decoder[Algorithm] = deriveDecoder
  given Encoder[Algorithm] =
    encoderWithType[Algorithm](using deriveEncoder[Algorithm])

  given Decoder[AbstractOperationHead] = deriveDecoder
  given Encoder[AbstractOperationHead] = encoderWithType[AbstractOperationHead](
    using deriveEncoder[AbstractOperationHead],
  )
  given Decoder[NumericMethodHead] = deriveDecoder
  given Encoder[NumericMethodHead] =
    encoderWithType[NumericMethodHead](using deriveEncoder[NumericMethodHead])
  given Decoder[SyntaxDirectedOperationHead] = deriveDecoder
  given Encoder[SyntaxDirectedOperationHead] =
    encoderWithType[SyntaxDirectedOperationHead](using
      deriveEncoder[SyntaxDirectedOperationHead],
    )
  given Decoder[ConcreteMethodHead] = deriveDecoder
  given Encoder[ConcreteMethodHead] =
    encoderWithType[ConcreteMethodHead](using deriveEncoder[ConcreteMethodHead])
  given Decoder[InternalMethodHead] = deriveDecoder
  given Encoder[InternalMethodHead] =
    encoderWithType[InternalMethodHead](using deriveEncoder[InternalMethodHead])
  given Decoder[BuiltinHead] = deriveDecoder
  given Encoder[BuiltinHead] =
    encoderWithType[BuiltinHead](using deriveEncoder[BuiltinHead])
  given Decoder[Head] = deriveDecoder
  given Encoder[Head] = Encoder.instance {
    case head: AbstractOperationHead       => head.asJson
    case head: NumericMethodHead           => head.asJson
    case head: SyntaxDirectedOperationHead => head.asJson
    case head: ConcreteMethodHead          => head.asJson
    case head: InternalMethodHead          => head.asJson
    case head: BuiltinHead                 => head.asJson
  }

  given Decoder[SdoHeadTarget] = deriveDecoder
  given Encoder[SdoHeadTarget] =
    encoderWithType[SdoHeadTarget](using deriveEncoder[SdoHeadTarget])
  given Decoder[BuiltinPath] = deriveDecoder
  given Encoder[BuiltinPath] = deriveEncoder

  // algorithm parameters
  given Decoder[Param] = deriveDecoder
  given Encoder[Param] = encoderWithType[Param](using deriveEncoder[Param])
  given Decoder[ParamKind] = deriveDecoder
  given Encoder[ParamKind] = deriveEncoder

  // type models
  given Decoder[TyModel] = deriveDecoder
  given Encoder[TyModel] = deriveEncoder
}
