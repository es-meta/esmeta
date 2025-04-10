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

  given Encoder[Reference] = encoderWithStringifier(stringify)
  given Decoder[Reference] = decoderWithParser(Reference.from)
  given Encoder[Property] = encoderWithStringifier(stringify)
  given Decoder[Property] = decoderWithParser(Property.from)
  given Encoder[Intrinsic] = encoderWithStringifier(stringify)
  given Decoder[Intrinsic] = decoderWithParser(Intrinsic.from)
  given Encoder[Expression] = encoderWithStringifier(stringify)
  given Decoder[Expression] = decoderWithParser(Expression.from)
  given Encoder[CalcExpression] = encoderWithStringifier(stringify)
  given Decoder[CalcExpression] = decoderWithParser(CalcExpression.from)
  given Encoder[BitwiseExpression] = encoderWithStringifier(stringify)
  given Decoder[BitwiseExpression] = decoderWithParser(BitwiseExpression.from)
  given Encoder[ClampExpression] = encoderWithStringifier(stringify)
  given Decoder[ClampExpression] = decoderWithParser(ClampExpression.from)
  given Encoder[MathOpExpression] = encoderWithStringifier(stringify)
  given Decoder[MathOpExpression] = decoderWithParser(MathOpExpression.from)
  given Encoder[Literal] = encoderWithStringifier(stringify)
  given Decoder[Literal] = decoderWithParser(Literal.from)
  given Encoder[Condition] = encoderWithStringifier(stringify)
  given Decoder[Condition] = decoderWithParser(Condition.from)
  given Encoder[Type] = encoderWithStringifier(stringify)
  given Decoder[Type] = decoderWithParser(Type.from)
  given Encoder[Block] = encoderWithStringifier(stringify)
  given Decoder[Block] = decoderWithParser(Block.from)
  given Encoder[Step] = encoderWithStringifier(stringify)
  given Decoder[Step] = decoderWithParser(Step.from)
  given Encoder[Directive] = deriveEncoder
  given Decoder[Directive] = deriveDecoder
}
