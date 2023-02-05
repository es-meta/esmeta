package esmeta
package ir
package util

import esmeta.util.*
import esmeta.util.BaseUtils.*
import io.circe.*, io.circe.syntax.*

object JsonProtocol extends BasicJsonProtocol {
  val stringifier = IRElem.getStringifier(true, false)
  import stringifier.given

  given typeEncoder: Encoder[Type] = encoderWithStringifier(stringify)
  given typeDecoder: Decoder[Type] = decoderWithParser(Type.from)
  given refEncoder: Encoder[Ref] = encoderWithStringifier(stringify)
  given refDecoder: Decoder[Ref] = decoderWithParser(Ref.from)
  given exprEncoder: Encoder[Expr] = encoderWithStringifier(stringify)
  given exprDecoder: Decoder[Expr] = decoderWithParser(Expr.from)
  given paramEncoder: Encoder[Param] = encoderWithStringifier(stringify)
  given paramDecoder: Decoder[Param] = decoderWithParser(Param.from)
  given funcEncoder: Encoder[Func] = encoderWithStringifier(stringify)
  given funcDecoder: Decoder[Func] = decoderWithParser(Func.from)
  given funcKindEncoder: Encoder[FuncKind] = encoderWithStringifier(stringify)
  given funcKindDecoder: Decoder[FuncKind] = decoderWithParser(FuncKind.from)
  given programEncoder: Encoder[Program] = encoderWithStringifier(stringify)
  given programDecoder: Decoder[Program] = decoderWithParser(Program.from)
  given instEncoder: Encoder[Inst] = encoderWithStringifier(stringify)
  given instDecoder: Decoder[Inst] = decoderWithParser(Inst.from)
  given uopEncoder: Encoder[UOp] = encoderWithStringifier(stringify)
  given uopDecoder: Decoder[UOp] = decoderWithParser(UOp.from)
  given bopEncoder: Encoder[BOp] = encoderWithStringifier(stringify)
  given bopDecoder: Decoder[BOp] = decoderWithParser(BOp.from)
  given vopEncoder: Encoder[VOp] = encoderWithStringifier(stringify)
  given vopDecoder: Decoder[VOp] = decoderWithParser(VOp.from)
  given mopEncoder: Encoder[MOp] = encoderWithStringifier(stringify)
  given mopDecoder: Decoder[MOp] = decoderWithParser(MOp.from)
  given copEncoder: Encoder[COp] = encoderWithStringifier(stringify)
  given copDecoder: Decoder[COp] = decoderWithParser(COp.from)
}
