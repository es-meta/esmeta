package esmeta.dump.util

import esmeta.*
import esmeta.cfg.CFG
import esmeta.cfg.util.{JsonProtocol as CFGJsonProtocol}
import esmeta.dump.*
import esmeta.lang.*
import esmeta.ir.*
import esmeta.spec.*
import esmeta.ty.*
import esmeta.util.BasicJsonProtocol

import io.circe.*, io.circe.syntax.*, io.circe.generic.semiauto.*

object DebuggerJsonProtocol extends BasicJsonProtocol {

  import esmeta.lang.util.JsonProtocol.given
  import esmeta.ir.util.JsonProtocol.given
  import esmeta.spec.util.JsonProtocol.given

  def ofCFG(cfg: CFG) = CFGJsonProtocol(cfg)

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

}
