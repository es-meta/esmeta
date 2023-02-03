package esmeta.util

import io.circe.*, io.circe.generic.semiauto.*
import io.circe.syntax.*

/** basic JSON protocols */
trait BasicJsonProtocol {

  given mapEncoder[K, V](using
    encodeK: Conversion[K, String],
    encodeV: Encoder[V],
  ): Encoder[Map[K, V]] = new Encoder {
    final def apply(map: Map[K, V]): Json =
      Json.fromFields(map.map((k, v) => encodeK(k) -> encodeV(v)))
  }

  given mapDecoder[K, V](using
    decodeK: Conversion[String, K],
    decodeV: Decoder[V],
  ): Decoder[Map[K, V]] = new Decoder {
    final def apply(c: HCursor): Decoder.Result[Map[K, V]] =
      c.as[Map[String, V]].map(_.map((s, v) => decodeK(s) -> v))
  }

  // decoder for UId: id -> UId
  def idDecoder[T <: UId](getter: Int => Option[T]): Decoder[T] =
    new Decoder[T] {
      final def apply(c: HCursor): Decoder.Result[T] = (for {
        number <- c.value.asNumber
        id <- number.toInt
        x <- getter(id)
      } yield Right(x)).getOrElse(unknownFail("id", c))
    }

  // encoder for UId: UId -> id
  def idEncoder[T <: UId]: Encoder[T] = new Encoder[T] {
    final def apply(x: T): Json = Json.fromInt(x.id)
  }

  // decoder for UId with name: { name: id } -> UId
  def idDecoderWithName[T <: UId](
    name: String,
    getter: Int => Option[T],
  ): Decoder[T] = new Decoder[T] {
    final def apply(c: HCursor): Decoder.Result[T] = (for {
      obj <- c.value.asObject
      value <- obj(name)
      number <- value.asNumber
      id <- number.toInt
      x <- getter(id)
    } yield Right(x)).getOrElse(unknownFail("id", c))
  }

  // encoder for UId with name: UId -> { name: id }
  def idEncoderWithName[T <: UId](name: String): Encoder[T] = new Encoder[T] {
    final def apply(x: T): Json =
      Json.fromFields(Seq(name -> Json.fromInt(x.id)))
  }

  // decoding failure
  def decodeFail[T](msg: String, c: HCursor): Decoder.Result[T] =
    Left(DecodingFailure(msg, c.history))

  // decoding failure
  def unknownFail[T](name: String, c: HCursor): Decoder.Result[T] =
    decodeFail(s"unknown $name: ${c.value}", c)
}
