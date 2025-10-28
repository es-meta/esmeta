package esmeta.util

import io.circe.*, io.circe.syntax.*, io.circe.generic.semiauto.*
import io.circe.DecodingFailure.Reason.WrongTypeExpectation
import scala.collection.mutable.{Map => MMap}
import scala.compiletime.constValue
import scala.compiletime.{erasedValue, summonFrom, error, constValue}
import scala.deriving.Mirror
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
      if (c.value.isNull) Right(None)
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

  // encoder for class A with fields {f1, ..., fn} serializes as
  // { A: {f1: v1, ..., fn: vn} }
  inline final def deriveEncoderWithType[A](using
    inline A: Mirror.Of[A],
  ): Encoder[A] = EncoderWithType.derived[A]

  // decoder for class A with fields {f1, ..., fn} serializes as
  // { A: {f1: v1, ..., fn: vn} }
  inline final def deriveDecoderWithType[A](using
    mirror: Mirror.Of[A],
  ): Decoder[A] = DecoderWithType.derived[A]
}

trait EncoderWithType[A] extends Encoder.AsObject[A] {
  lazy val elemLabels: List[String]
  lazy val elemEncoders: List[Encoder[?]]

  final def encodeElemAt(index: Int, elem: Any): (String, Json) = (
    elemLabels(index),
    elemEncoders(index).asInstanceOf[Encoder[Any]].apply(elem),
  )

  final def encodeProduct(name: String, a: A): JsonObject =
    val product = a.asInstanceOf[Product]
    val iterable = Iterable.tabulate(product.productArity) { index =>
      encodeElemAt(index, product.productElement(index))
    }
    val json = JsonObject.fromIterable(iterable).toJson
    // XXX: add type name (i.e., { name: { ... } })
    JsonObject.singleton(name, json)

  final def encodeSum(index: Int, a: A): JsonObject =
    val (_, json) = encodeElemAt(index, a)
    // XXX: without discriminator
    json.asObject.getOrElse(JsonObject.empty)
}

object EncoderWithType {
  private def of[A](
    name: String,
    encoders: => List[Encoder[?]],
    labels: List[String],
  )(using
    mirror: Mirror.Of[A],
  ): EncoderWithType[A] = mirror match
    case mirror: Mirror.ProductOf[A] =>
      new EncoderWithType[A]:
        lazy val elemEncoders = encoders
        lazy val elemLabels = labels
        def isSum = false
        def encodeObject(a: A) = encodeProduct(name, a)
    case mirror: Mirror.SumOf[A] =>
      new EncoderWithType[A]:
        lazy val elemEncoders = encoders
        lazy val elemLabels = labels
        def isSum = true
        def encodeObject(a: A) = encodeSum(mirror.ordinal(a), a)

  private inline final def encoders[A](using
    mirror: Mirror.Of[A],
  ): List[Encoder[?]] =
    summonEncoders[mirror.MirroredElemTypes](derivingForSum =
      inline mirror match {
        case _: Mirror.ProductOf[A] => false
        case _: Mirror.SumOf[A]     => true
      },
    )

  inline final def derived[A](using mirror: Mirror.Of[A]): EncoderWithType[A] =
    val name = constValue[mirror.MirroredLabel]
    EncoderWithType
      .of[A](name, encoders[A], summonLabels[mirror.MirroredElemLabels])

  private inline final def summonEncoders[T <: Tuple](
    inline derivingForSum: Boolean,
  ): List[Encoder[_]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts) =>
        summonEncoder[t](derivingForSum) :: summonEncoders[ts](derivingForSum)

  private inline final def summonEncoder[A](
    inline derivingForSum: Boolean,
  ): Encoder[A] =
    summonFrom {
      case encodeA: Encoder[A] => encodeA
      case m: Mirror.Of[A] =>
        inline if (derivingForSum) EncoderWithType.derived[A]
        else error("Failed to find an instance of Encoder[" + typeName[A] + "]")
    }
}

trait DecoderWithType[A] extends Decoder[A] {
  val name: String
  lazy val elemLabels: List[String]
  lazy val elemDecoders: List[Decoder[?]]
  def isSum: Boolean

  private lazy val decodersDict: Map[String, Decoder[?]] = {
    def findDecoderDict(p: (String, Decoder[?])): List[(String, Decoder[?])] =
      p._2 match {
        case cd: DecoderWithType[?] if cd.isSum =>
          cd.elemLabels.zip(cd.elemDecoders).flatMap(findDecoderDict)
        case _ => List(p)
      }
    elemLabels.zip(elemDecoders).flatMap(findDecoderDict).toMap
  }

  /** Decodes a class/object/case of a Sum type handling discriminator and
    * strict decoding.
    */
  private def decodeSumElement[R](c: HCursor)(
    fail: DecodingFailure => R,
    decode: Decoder[A] => ACursor => R,
  ): R = c.keys match {
    case None =>
      fail(
        DecodingFailure(
          WrongTypeExpectation("object", c.value),
          c.history,
        ),
      )
    case Some(keys) =>
      val iter = keys.iterator
      if (!iter.hasNext)
        fail(
          DecodingFailure(
            WrongTypeExpectation("non-empty json object", c.value),
            c.history,
          ),
        )
      else
        val sumTypeName = iter.next
        decodersDict
          .get(sumTypeName)
          .fold(
            fail(
              DecodingFailure(
                s"type $name has no class/object/case named '$sumTypeName'.",
                c.history,
              ),
            ),
          ) { decoder => decode(decoder.asInstanceOf[Decoder[A]])(c) }
  }

  final def decodeSum(c: HCursor): Decoder.Result[A] =
    decodeSumElement(c)(Left.apply, _.tryDecode)

  final def decodeProduct(
    c: HCursor,
    fromProduct: Product => A,
  ): Decoder.Result[A] = c.downField(name).success match {
    case Some(c) =>
      if (!c.value.isObject)
        Left(
          DecodingFailure(WrongTypeExpectation("object", c.value), c.history),
        )
      else {
        val res = new Array[Any](elemLabels.length)
        var failed: Left[DecodingFailure, _] = null
        var index = 0
        while (index < elemLabels.length && (failed eq null)) {
          decodeProductElement(c, index, _.tryDecode) match
            case Right(value) => res(index) = value
            case l @ Left(_)  => failed = l
          index += 1
        }
        if (failed eq null) Right(fromProduct(Tuple.fromArray(res)))
        else failed.asInstanceOf[Decoder.Result[A]]
      }
    case _ =>
      Left(
        DecodingFailure(
          s"expected an object with key '$name' but found: ${c.value}",
          c.history,
        ),
      )
  }

  // Decodes a single element of a product.
  private def decodeProductElement[R](
    c: HCursor,
    index: Int,
    decode: Decoder[Any] => ACursor => R,
  ): R =
    val decoder = elemDecoders(index).asInstanceOf[Decoder[Any]]
    val cursor = c.downField(elemLabels(index))
    decode(decoder)(cursor)
}

object DecoderWithType {
  private def of[A](
    nme: String,
    decoders: => List[Decoder[?]],
    labels: List[String],
  )(using mirror: Mirror.Of[A]): DecoderWithType[A] = mirror match
    case mirror: Mirror.ProductOf[A] =>
      new DecoderWithType[A]:
        val name = nme
        lazy val elemDecoders = decoders
        lazy val elemLabels = labels
        def isSum = false
        def apply(c: HCursor) = decodeProduct(c, mirror.fromProduct)
    case _: Mirror.SumOf[A] =>
      new DecoderWithType[A]:
        val name = nme
        lazy val elemDecoders = decoders
        lazy val elemLabels = labels
        def isSum = true
        def apply(c: HCursor) = decodeSum(c)

  private inline final def decoders[A](using
    mirror: Mirror.Of[A],
  ): List[Decoder[?]] =
    summonDecoders[mirror.MirroredElemTypes](derivingForSum =
      inline mirror match {
        case _: Mirror.ProductOf[A] => false
        case _: Mirror.SumOf[A]     => true
      },
    )

  inline final def derived[A](using mirror: Mirror.Of[A]): DecoderWithType[A] =
    DecoderWithType.of[A](
      constValue[mirror.MirroredLabel],
      decoders[A],
      summonLabels[mirror.MirroredElemLabels],
    )

  private inline final def summonDecoders[T <: Tuple](
    inline derivingForSum: Boolean,
  ): List[Decoder[_]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts) =>
        summonDecoder[t](derivingForSum) :: summonDecoders[ts](derivingForSum)

  private inline final def summonDecoder[A](
    inline derivingForSum: Boolean,
  ): Decoder[A] =
    summonFrom {
      case decodeA: Decoder[A] => decodeA
      case m: Mirror.Of[A] =>
        inline if (derivingForSum) DecoderWithType.derived[A]
        else error("Failed to find an instance of Decoder[" + typeName[A] + "]")
    }
}

private inline final def summonLabels[T <: Tuple]: List[String] =
  inline erasedValue[T] match
    case _: EmptyTuple => Nil
    case _: (t *: ts) =>
      constValue[t].asInstanceOf[String] :: summonLabels[ts]
