package esmeta
package ir
package util

import esmeta.lang.Syntax
import esmeta.lang.util.JsonProtocol.given
import esmeta.spec.util.JsonProtocol.given
import esmeta.util.*
import esmeta.util.BaseUtils.*
import io.circe.*, io.circe.syntax.*, io.circe.generic.semiauto.*
import scala.util.Try

object JsonProtocolWithBackEdge extends BasicJsonProtocol {
  val stringifier = IRElem.getStringifier(true, false)
  import stringifier.given

  given Encoder[Type] = encoderWithStringifier(stringify)
  given Decoder[Type] = decoderWithParser(Type.from)

  given Encoder[Ref] = encoderWithStringifier(stringify)
  given Decoder[Ref] = decoderWithParser(Ref.from)
  given Encoder[Expr] = encoderWithStringifier(stringify)
  given Decoder[Expr] = decoderWithParser(Expr.from)
  given Encoder[Param] = encoderWithStringifier(stringify)
  given Decoder[Param] = decoderWithParser(Param.from)
  given Encoder[Func] = deriveEncoder
  given Decoder[Func] = deriveDecoder
  given Encoder[FuncKind] = deriveEncoder
  given Decoder[FuncKind] = deriveDecoder
  given Encoder[Program] = encoderWithStringifier(stringify)
  given Decoder[Program] = decoderWithParser(Program.from)

  // given Encoder[Inst] = deriveEncoder
  // given Decoder[Inst] = deriveDecoder

  given Encoder[Pos] = deriveEncoder
  given Decoder[Pos] = deriveDecoder
  given Encoder[Loc] = deriveEncoder
  given Decoder[Loc] = deriveDecoder

  val LANG_OPT_LOC = "langOptLoc"

  given Encoder[Inst] =
    (i: Inst) =>
      deriveEncoder[Inst]
        .apply(i)
        .mapObject(o =>
          o.+:(
            (LANG_OPT_LOC -> i.langOpt.fold(None)(_.loc).asJson),
          ),
        )

  given Decoder[Inst] = Decoder.instance { cursor =>
    for
      langOptLoc <- cursor.downField(LANG_OPT_LOC).as[Option[Loc]]
      inst <- Decoder
        .instance { inner =>
          {
            inner
              .downField(LANG_OPT_LOC)
              .delete
              .as[Inst](using deriveDecoder[Inst])
          }
        }
        .apply(cursor)
    yield {
      inst.setLang(new Syntax { loc = langOptLoc })
      inst
    }
  }

  given Encoder[Name] = deriveEncoder
  given Decoder[Name] = deriveDecoder
  given Encoder[Local] = deriveEncoder
  given Decoder[Local] = deriveDecoder

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
