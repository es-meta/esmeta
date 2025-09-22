package esmeta.util

import scala.collection.mutable.{Map => MMap}
import io.circe.*, io.circe.generic.semiauto.*
import io.circe.syntax.*
import scala.reflect.ClassTag

/** basic JSON protocols */
trait BasicJsonProtocol {
  // decoder for map structures with string keys
  def strMapDecoder[V](using decoder: Decoder[V]): Decoder[Map[String, V]] =
    Decoder.instance(_.as[Map[String, V]])

  // encoder for map structures with string keys
  def strMapEncoder[V](using encoder: Encoder[V]): Encoder[Map[String, V]] =
    Encoder.instance { map =>
      Json.fromFields(map.toList.sortBy(_._1).map { (k, v) => (k, v.asJson) })
    }

  // decoder for double values
  def doubleDecoder: Decoder[Double] = Decoder.instance(c =>
    c.value.asString
      .map(_ match
        case "Infinity"  => Double.PositiveInfinity
        case "-Infinity" => Double.NegativeInfinity
        case "NaN"       => Double.NaN,
      )
      .orElse(c.value.asNumber.map(_.toDouble))
      .map(Right(_))
      .getOrElse(invalidFail("double", c)),
  )

  // encoder for double values
  def doubleEncoder: Encoder[Double] =
    Encoder.instance(Json.fromDoubleOrString)

  // decoder for option
  def optionDecoder[T](using decoder: Decoder[T]): Decoder[Option[T]] =
    Decoder.instance(c =>
      if c.value.isNull then Right(None)
      else decoder(c).map(Some(_)),
    )

  // encoder for option
  def optionEncoder[T](using encoder: Encoder[T]): Encoder[Option[T]] =
    Encoder.instance(opt => opt.fold(Json.Null)(encoder.apply))

  // decoder for UId: id -> UId
  def uidDecoder[T <: UId](getter: Int => Option[T]): Decoder[T] =
    Decoder.instance(c =>
      (for {
        number <- c.value.asNumber
        id <- number.toInt
        x <- getter(id)
      } yield Right(x)).getOrElse(invalidFail("id", c)),
    )

  // encoder for UId: UId -> id
  def uidEncoder[T <: UId]: Encoder[T] =
    Encoder.instance(x => Json.fromInt(x.id))

  // decoder for UId with name: { name: id } -> UId
  def uidDecoderWithName[T <: UId](
    name: String,
    getter: Int => Option[T],
  ): Decoder[T] = Decoder.instance(c =>
    (for {
      obj <- c.value.asObject
      value <- obj(name)
      number <- value.asNumber
      id <- number.toInt
      x <- getter(id)
    } yield Right(x)).getOrElse(invalidFail("id", c)),
  )

  // encoder for UId with name: UId -> { name: id }
  def uidEncoderWithName[T <: UId](name: String): Encoder[T] =
    Encoder.instance(x => Json.fromFields(Seq(name -> Json.fromInt(x.id))))

  // encoder based on stringifiers
  def encoderWithStringifier[T](stringifier: T => String): Encoder[T] =
    Encoder.instance(x => Json.fromString(stringifier(x)))

  // encoder for class A with fields {f1, ..., fn} serializes as { A: {f1: v1, ..., fn: vn} }
  def encoderWithType[A](using enc: Encoder[A], ct: ClassTag[A]): Encoder[A] =
    Encoder.instance { a =>
      Json.obj(ct.runtimeClass.getSimpleName -> enc(a))
    }

  // encoder based on discrimators
  def decoderWithDiscriminator[T](
    name: String,
    discriminators: List[(String, HCursor => Decoder.Result[T])],
  ): Decoder[T] = Decoder.instance(c =>
    (for {
      obj <- c.value.asObject
      res <- discriminators.foldLeft[Option[Decoder.Result[T]]](None) {
        case (None, (key, decoder)) if obj.contains(key) => Some(decoder(c))
        case (res, _)                                    => res
      }
    } yield res).getOrElse(invalidFail(name, c)),
  )

  // decoder based on parsers
  def decoderWithParser[T](parser: String => T): Decoder[T] =
    Decoder.instance(c =>
      c.value.asString
        .map(parser)
        .map(Right(_))
        .getOrElse(decodeFail(s"expected a string instead of ${c.value}", c)),
    )

  // decoding failure
  def decodeFail[T](msg: String, c: HCursor): Decoder.Result[T] =
    Left(DecodingFailure(msg, c.history))

  // decoding failure
  def invalidFail[T](name: String, c: HCursor): Decoder.Result[T] =
    decodeFail(s"invalid $name: ${c.value}", c)
}
