package esmeta.dump.debugger.util

import esmeta.*
import esmeta.cfg.CFG
import esmeta.dump.debugger.DUMMY_BODY
import esmeta.lang.*
import esmeta.ir.*
import esmeta.spec.*
import esmeta.ty.*
import esmeta.util.BasicJsonProtocol
import esmeta.web.util.JsonProtocol as WebJsonProtocol
import io.circe.*, io.circe.syntax.*, io.circe.generic.semiauto.*

class JsonProtocol(cfg: CFG) extends WebJsonProtocol(cfg) {

  import esmeta.lang.util.JsonProtocol.given
  import esmeta.ir.util.JsonProtocol.given
  import esmeta.spec.util.JsonProtocol.given

  val algorithmOmittedDecoder: Decoder[Algorithm] = Decoder.instance { c =>
    for {
      head <- c.downField("head").as[Head]
      code <- c.downField("code").as[String]
    } yield Algorithm(head, DUMMY_BODY, code)
  }
  val algorithmOmittedEncoder: Encoder[Algorithm] = Encoder.instance { alg =>
    Json.obj(
      "head" -> alg.head.asJson,
      "code" -> alg.code.asJson,
    )
  }

  given Decoder[Func] = {
    given Decoder[Algorithm] = algorithmOmittedDecoder
    deriveDecoder[Func]
  }
  given Encoder[Func] = {
    given Encoder[Algorithm] = algorithmOmittedEncoder
    deriveEncoder[Func]
  }

  given Decoder[Spec.Version] = summon
  given Encoder[Spec.Version] = summon

  given Decoder[Table] = summon
  given Encoder[Table] = summon

  given Decoder[ty.TyModel] = summon
  given Encoder[ty.TyModel] = summon

  given Decoder[Grammar] = summon
  given Encoder[Grammar] = summon

  given Decoder[lang.Type] = summon
  given Encoder[lang.Type] = summon

}
