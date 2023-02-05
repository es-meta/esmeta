package esmeta
package lang
package util

import esmeta.util.*
import esmeta.util.BaseUtils.*
import io.circe.*, io.circe.syntax.*

object JsonProtocol extends BasicJsonProtocol {
  val stringifier = LangElem.getStringifier(true, false)
  import stringifier.given

  given refEncoder: Encoder[Reference] = encoderWithStringifier(stringify)
  given refDecoder: Decoder[Reference] = decoderWithParser(Reference.from)
  given propEncoder: Encoder[Property] = encoderWithStringifier(stringify)
  given propDecoder: Decoder[Property] = decoderWithParser(Property.from)
  given intrEncoder: Encoder[Intrinsic] = encoderWithStringifier(stringify)
  given intrDecoder: Decoder[Intrinsic] = decoderWithParser(Intrinsic.from)
  given exprEncoder: Encoder[Expression] = encoderWithStringifier(stringify)
  given exprDecoder: Decoder[Expression] = decoderWithParser(Expression.from)
  given condEncoder: Encoder[Condition] = encoderWithStringifier(stringify)
  given condDecoder: Decoder[Condition] = decoderWithParser(Condition.from)
  given typeEncoder: Encoder[Type] = encoderWithStringifier(stringify)
  given typeDecoder: Decoder[Type] = decoderWithParser(Type.from)
  given blockEncoder: Encoder[Block] = encoderWithStringifier(stringify)
  given blockDecoder: Decoder[Block] = decoderWithParser(Block.from)
  given stepEncoder: Encoder[Step] = encoderWithStringifier(stringify)
  given stepDecoder: Decoder[Step] = decoderWithParser(Step.from)
}
