package esmeta
package ty
package util

import esmeta.util.*
import esmeta.util.BaseUtils.*
import io.circe.*, io.circe.syntax.*, io.circe.generic.semiauto.*

object JsonProtocol extends BasicJsonProtocol {
  import Stringifier.given

  given Decoder[CompTy] = decoderWithParser(CompTy.from)
  given Encoder[CompTy] = encoderWithStringifier(stringify)
  given Decoder[BoolTy] = decoderWithParser(BoolTy.from)
  given Encoder[BoolTy] = encoderWithStringifier(stringify)
  given Decoder[MapTy] = decoderWithParser(MapTy.from)
  given Encoder[MapTy] = encoderWithStringifier(stringify)
  given Decoder[RecordTy] = decoderWithParser(RecordTy.from)
  given Encoder[RecordTy] = encoderWithStringifier(stringify)
  given Decoder[ListTy] = decoderWithParser(ListTy.from)
  given Encoder[ListTy] = encoderWithStringifier(stringify)
  given Decoder[ValueTy] = decoderWithParser(ValueTy.from)
  given Encoder[ValueTy] = encoderWithStringifier(stringify)
  given Decoder[AstValueTy] = decoderWithParser(AstValueTy.from)
  given Encoder[AstValueTy] = encoderWithStringifier(stringify)
  given Decoder[PureValueTy] = decoderWithParser(PureValueTy.from)
  given Encoder[PureValueTy] = encoderWithStringifier(stringify)
  given Decoder[Ty] = decoderWithParser(Ty.from)
  given Encoder[Ty] = encoderWithStringifier(stringify)
  given Decoder[NameTy] = decoderWithParser(NameTy.from)
  given Encoder[NameTy] = encoderWithStringifier(stringify)
  given [T](using decoder: Decoder[T]): Decoder[Map[String, T]] = strMapDecoder
  given [T](using encoder: Encoder[T]): Encoder[Map[String, T]] = strMapEncoder

  // type modeling
  given Decoder[TyModel] = deriveDecoder
  given Encoder[TyModel] = deriveEncoder
  given Decoder[TyInfo] = deriveDecoder
  given Encoder[TyInfo] = deriveEncoder
}
