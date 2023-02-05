package esmeta
package spec
package util

import esmeta.util.*
import esmeta.util.BaseUtils.*
import io.circe.*, io.circe.syntax.*, io.circe.generic.semiauto.*
import lang.util.JsonProtocol.given

object JsonProtocol extends BasicJsonProtocol {
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
    "nonCases" -> (_.as[ButNot]),
    "methodName" -> (_.as[ButOnlyIf]),
    "cases" -> (_.as[Lookahead]),
    "empty" -> (_ => Right(Empty)),
    "nlt" -> (_ => Right(NoLineTerminator)),
    "abbr" -> (_.as[CodePointAbbr]),
    "cpCond" -> (_.as[UnicodeSet]),
  )
  given Encoder[Symbol] = Encoder.instance {
    case symbol: Terminal      => symbol.asJson
    case symbol: Nonterminal   => symbol.asJson
    case symbol: ButNot        => symbol.asJson
    case symbol: ButOnlyIf     => symbol.asJson
    case symbol: Lookahead     => symbol.asJson
    case Empty                 => Json.obj("empty" -> Json.Null)
    case NoLineTerminator      => Json.obj("nlt" -> Json.Null)
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
  given Decoder[ButNot] = deriveDecoder
  given Encoder[ButNot] = deriveEncoder
  given Decoder[ButOnlyIf] = deriveDecoder
  given Encoder[ButOnlyIf] = deriveEncoder
  given Decoder[Lookahead] = deriveDecoder
  given Encoder[Lookahead] = deriveEncoder
  given Decoder[CodePointAbbr] = deriveDecoder
  given Encoder[CodePointAbbr] = deriveEncoder
  given Decoder[UnicodeSet] = deriveDecoder
  given Encoder[UnicodeSet] = deriveEncoder

  // ---------------------------------------------------------------------------
  // Algorithms
  // ---------------------------------------------------------------------------
  // algorithm heads
  given Decoder[Head] =
    decoderWithDiscriminator("algorithm head", headDiscriminators)
  private lazy val headDiscriminators
    : List[(String, HCursor => Decoder.Result[Head])] = List(
    "isHostDefined" -> (_.as[AbstractOperationHead]),
    "baseTy" -> (_.as[NumericMethodHead]),
    "withParams" -> (_.as[SyntaxDirectedOperationHead]),
    "concMethodName" -> (_.as[ConcreteMethodHead]),
    "receiverParam" -> (_.as[InternalMethodHead]),
    "path" -> (_.as[BuiltinHead]),
  )
  given Encoder[Head] = Encoder.instance {
    case head: AbstractOperationHead       => head.asJson
    case head: NumericMethodHead           => head.asJson
    case head: SyntaxDirectedOperationHead => head.asJson
    case head: ConcreteMethodHead          => head.asJson
    case head: InternalMethodHead          => head.asJson
    case head: BuiltinHead                 => head.asJson
  }
  given Decoder[AbstractOperationHead] = deriveDecoder
  given Encoder[AbstractOperationHead] = deriveEncoder
  given Decoder[NumericMethodHead] = deriveDecoder
  given Encoder[NumericMethodHead] = deriveEncoder
  given Decoder[SyntaxDirectedOperationHead] = deriveDecoder
  given Encoder[SyntaxDirectedOperationHead] = deriveEncoder
  given Decoder[SdoHeadTarget] = deriveDecoder
  given Encoder[SdoHeadTarget] = deriveEncoder
  given Decoder[ConcreteMethodHead] = deriveDecoder
  given Encoder[ConcreteMethodHead] = deriveEncoder
  given Decoder[InternalMethodHead] = deriveDecoder
  given Encoder[InternalMethodHead] = deriveEncoder
  given Decoder[BuiltinHead] = deriveDecoder
  given Encoder[BuiltinHead] = deriveEncoder
  given Decoder[BuiltinPath] = deriveDecoder
  given Encoder[BuiltinPath] = deriveEncoder
  // algorithm parameters
  given Decoder[Param] = deriveDecoder
  given Encoder[Param] = deriveEncoder
  given Decoder[ParamKind] = deriveDecoder
  given Encoder[ParamKind] = deriveEncoder
}
