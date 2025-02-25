package esmeta
package ty
package util

import esmeta.lang.Syntax
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.cfg.*
import esmeta.cfg.util.{JsonProtocol => CFGJsonProtocol}
import esmeta.spec.*
import esmeta.es.util.Node
import esmeta.ir.IReturn
import esmeta.fuzzer.TypeErrorRecord
import esmeta.ir.util.JsonProtocol.given
import esmeta.ty.util.JsonProtocol.given
import io.circe.*, io.circe.syntax.*, io.circe.generic.semiauto.*

class JsonProtocol2(cfg: CFG) extends CFGJsonProtocol(cfg) {
  // type errors
  given Encoder[TypeErrorRecord] = deriveEncoder
  given Encoder[TypeError] = Encoder.instance {
    case p: ParamTypeMismatch  => p.asJson // 필드만 JSON으로 변환
    case r: ReturnTypeMismatch => r.asJson
    case _                     => Json.obj("ToDo" -> "ToDo".asJson)
  }

  given Encoder[ParamTypeMismatch] = deriveEncoder
  given Encoder[ReturnTypeMismatch] = deriveEncoder

  given Decoder[TypeErrorRecord] = deriveDecoder
  given Decoder[TypeError] =
    Decoder[ParamTypeMismatch].either(Decoder[ReturnTypeMismatch]).map(_.merge)

  given Decoder[ParamTypeMismatch] = deriveDecoder
  given Decoder[ReturnTypeMismatch] = deriveDecoder
  given Decoder[ArityMismatch] = ???
  given Decoder[InvalidBaseError] = ???
  given Decoder[UnaryOpTypeMismatch] = ???
  given Decoder[BinaryOpTypeMismatch] = ???

  // type error points
  given cpEncoder: Encoder[CallPoint] = Encoder.instance {
    case CallPoint(caller, callsite, callee) =>
      Json.obj(
        "caller" -> caller.name.asJson,
        "callsite" -> callsite.asJson,
        "callee" -> callee.name.asJson,
      )
  }

  given cpDecoder: Decoder[CallPoint] = Decoder.instance { cursor =>
    for {
      caller <- cursor.downField("caller").as[String].map(cfg.getFunc)
      callsite <- cursor.downField("callsite").as[Call]
      callee <- cursor.downField("callee").as[String].map(cfg.getFunc)
    } yield CallPoint(caller, callsite, callee)
  }

  // ToDo: handle param name
  given aapEncoder: Encoder[ArgAssignPoint] = deriveEncoder
  given aapDecoder: Decoder[ArgAssignPoint] = deriveDecoder

  given iipEncoder: Encoder[InternalReturnPoint] = Encoder.instance {
    case InternalReturnPoint(func, node, irReturn) =>
      Json.obj(
        "func" -> func.name.asJson,
        "node" -> nodeEncoder(node),
        "irReturn" -> irReturn.asJson,
      )
  }
  given iipDecoder: Decoder[InternalReturnPoint] = Decoder.instance { cursor =>
    for {
      func <- cursor.downField("func").as[String].map(cfg.getFunc)
      node <- cursor.downField("node").as[Node]
      irReturn <- cursor.downField("irReturn").as[IReturn]
    } yield InternalReturnPoint(func, node, irReturn)
  }
}
