package esmeta
package ir
package util

import esmeta.util.*
import esmeta.util.BaseUtils.*
import io.circe.*, io.circe.syntax.*

object JsonProtocol extends BasicJsonProtocol {
  val stringifier = IRElem.getStringifier(true, true)
  import stringifier.given

  given Encoder[Type] = encoderWithStringifier(stringify)
  given Decoder[Type] = decoderWithParser(Type.from)
  given Encoder[Ref] = encoderWithStringifier(stringify)
  given Decoder[Ref] = decoderWithParser(Ref.from)
  given Encoder[Expr] = encoderWithStringifier(stringify)
  given Decoder[Expr] = decoderWithParser(Expr.from)
  given Encoder[Param] = encoderWithStringifier(stringify)
  given Decoder[Param] = decoderWithParser(Param.from)
  given Encoder[Func] = encoderWithStringifier(stringify)
  given Decoder[Func] = decoderWithParser(Func.from)
  given Encoder[FuncKind] = encoderWithStringifier(stringify)
  given Decoder[FuncKind] = decoderWithParser(FuncKind.from)
  given Encoder[Program] = encoderWithStringifier(stringify)
  given Decoder[Program] = decoderWithParser(Program.from)
  given Encoder[Inst] = encoderWithStringifier(stringify)
  given Decoder[Inst] = decoderWithParser(Inst.from)
  given Encoder[UOp] = encoderWithStringifier(stringify)
  given Decoder[UOp] = decoderWithParser(UOp.from)
  given Encoder[BOp] = encoderWithStringifier(stringify)
  given Decoder[BOp] = decoderWithParser(BOp.from)
  given Encoder[VOp] = encoderWithStringifier(stringify)
  given Decoder[VOp] = decoderWithParser(VOp.from)
  given Encoder[MOp] = encoderWithStringifier(stringify)
  given Decoder[MOp] = decoderWithParser(MOp.from)
  given Encoder[COp] = encoderWithStringifier(stringify)
  given Decoder[COp] = decoderWithParser(COp.from)
}
