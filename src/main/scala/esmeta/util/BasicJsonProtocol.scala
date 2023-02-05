package esmeta.util

import scala.collection.mutable.{Map => MMap}
import io.circe.*, io.circe.generic.semiauto.*
import io.circe.syntax.*

/** basic JSON protocols */
trait BasicJsonProtocol {

  // encoder for map structures
  given mapEncoder[K, V](using
    kEncoder: Encoder[K],
    vEncoder: Encoder[V],
  ): Encoder[Map[K, V]] =
    Encoder.instance(map => Json.fromValues(map.map(_.asJson)))

  // decoder for map structures
  given mapDecoder[K, V](using
    kDecoder: Decoder[K],
    vDecoder: Decoder[V],
  ): Decoder[Map[K, V]] =
    Decoder.instance(_.as[Vector[(K, V)]].map(_.toMap))

  // encoder for mutable map structures
  given mmapEncoder[K, V](using
    kEncoder: Encoder[K],
    vEncoder: Encoder[V],
  ): Encoder[MMap[K, V]] =
    Encoder.instance(map => Json.fromValues(map.map(_.asJson)))

  // decoder for mutable map structures
  given mmapDecoder[K, V](using
    kDecoder: Decoder[K],
    vDecoder: Decoder[V],
  ): Decoder[MMap[K, V]] =
    Decoder.instance(_.as[Vector[(K, V)]].map(MMap.from))

  // decoder for double values
  given doubleDecoder: Decoder[Double] = new Decoder[Double] {
    final def apply(c: HCursor): Decoder.Result[Double] = {
      c.value.asString
        .map(_ match
          case "Infinity"  => Double.PositiveInfinity
          case "-Infinity" => Double.NegativeInfinity
          case "NaN"       => Double.NaN,
        )
        .orElse(c.value.asNumber.map(_.toDouble))
        .map(Right(_))
        .getOrElse(invalidFail("double", c))
    }
  }

  // encoder for double values
  given doubleEncoder: Encoder[Double] =
    Encoder.instance(Json.fromDoubleOrString)

  // decoder for UId: id -> UId
  def uidDecoder[T <: UId](getter: Int => Option[T]): Decoder[T] =
    new Decoder[T] {
      final def apply(c: HCursor): Decoder.Result[T] = (for {
        number <- c.value.asNumber
        id <- number.toInt
        x <- getter(id)
      } yield Right(x)).getOrElse(invalidFail("id", c))
    }

  // encoder for UId: UId -> id
  def uidEncoder[T <: UId]: Encoder[T] = new Encoder[T] {
    final def apply(x: T): Json = Json.fromInt(x.id)
  }

  // decoder for UId with name: { name: id } -> UId
  def uidDecoderWithName[T <: UId](
    name: String,
    getter: Int => Option[T],
  ): Decoder[T] = new Decoder[T] {
    final def apply(c: HCursor): Decoder.Result[T] = (for {
      obj <- c.value.asObject
      value <- obj(name)
      number <- value.asNumber
      id <- number.toInt
      x <- getter(id)
    } yield Right(x)).getOrElse(invalidFail("id", c))
  }

  // encoder based on stringifiers
  def encoderWithStringifier[T](stringifier: T => String): Encoder[T] =
    Encoder.instance(x => Json.fromString(stringifier(x)))

  // decoder based on parsers
  def decoderWithParser[T](parser: String => T): Decoder[T] =
    Decoder.instance(c =>
      c.value.asString
        .map(parser)
        .map(Right(_))
        .getOrElse(decodeFail(s"expected a string instead of ${c.value}", c)),
    )

  // encoder for UId with name: UId -> { name: id }
  def uidEncoderWithName[T <: UId](name: String): Encoder[T] = new Encoder[T] {
    final def apply(x: T): Json =
      Json.fromFields(Seq(name -> Json.fromInt(x.id)))
  }

  // decoding failure
  def decodeFail[T](msg: String, c: HCursor): Decoder.Result[T] =
    Left(DecodingFailure(msg, c.history))

  // decoding failure
  def invalidFail[T](name: String, c: HCursor): Decoder.Result[T] =
    decodeFail(s"invalid $name: ${c.value}", c)
}
