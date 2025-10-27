package esmeta
package spec
package util

import esmeta.es.ESElem
import esmeta.es.builtin.Intrinsics
import esmeta.lang.util.JsonProtocol.given
import esmeta.ty.*
import esmeta.ty.util.JsonProtocol.given
import esmeta.util.*
import esmeta.util.BaseUtils.*
import io.circe.*, io.circe.syntax.*, io.circe.generic.semiauto.*

object JsonProtocol extends BasicJsonProtocol {
  private val stringifier = ESElem.getStringifier(true, false, None)
  import stringifier.given

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
  given Decoder[Algorithm] = deriveDecoderWithType
  given Encoder[Algorithm] = deriveEncoderWithType

  given Decoder[AbstractOperationHead] = deriveDecoderWithType
  given Encoder[AbstractOperationHead] = deriveEncoderWithType
  given Decoder[NumericMethodHead] = deriveDecoderWithType
  given Encoder[NumericMethodHead] = deriveEncoderWithType
  given Decoder[SyntaxDirectedOperationHead] = deriveDecoderWithType
  given Encoder[SyntaxDirectedOperationHead] = deriveEncoderWithType
  given Decoder[ConcreteMethodHead] = deriveDecoderWithType
  given Encoder[ConcreteMethodHead] = deriveEncoderWithType
  given Decoder[InternalMethodHead] = deriveDecoderWithType
  given Encoder[InternalMethodHead] = deriveEncoderWithType
  given Decoder[BuiltinHead] = deriveDecoderWithType
  given Encoder[BuiltinHead] = deriveEncoderWithType
  given Decoder[Head] = deriveDecoderWithType
  given Encoder[Head] = deriveEncoderWithType

  given Decoder[SdoHeadTarget] = deriveDecoderWithType
  given Encoder[SdoHeadTarget] = deriveEncoderWithType
  given Decoder[BuiltinPath] = deriveDecoderWithType
  given Encoder[BuiltinPath] = deriveEncoderWithType

  // algorithm parameters
  given Decoder[Param] = deriveDecoderWithType
  given Encoder[Param] = deriveEncoderWithType
  given Decoder[ParamKind] = deriveDecoderWithType
  given Encoder[ParamKind] = deriveEncoderWithType

  // type models
  given Decoder[TyModel] = deriveDecoderWithType
  given Encoder[TyModel] = deriveEncoderWithType

  // intrinsics
  given Decoder[Intrinsics] = decoderWithParser(Intrinsics.from)
  given Encoder[Intrinsics] = encoderWithStringifier(stringify)
}
