package esmeta.parser

import esmeta.*
import esmeta.util.SystemUtils.*
import io.circe.*, io.circe.syntax.*, io.circe.parser.*

object Unicode {
  val RECENT_VERSION = "16.0.0"

  lazy val IDStart = get("ID_Start")
  lazy val IDContinue = get("ID_Continue")

  /** unicode set getter
    *
    * TODO: utilize npm module `@es-meta/unicode-gen`
    */
  def get(property: String, version: String = RECENT_VERSION): Set[Int] =
    val filename = s"$UNICODE_DIR/${property}_$version.json"
    val elems = readJson[List[Elem]](filename)
    elems.flatMap(decode).toSet

  /** compressed unicode set elements */
  enum Elem:
    case Code(code: Int)
    case Range(start: Int, end: Int)

  /** decode compressed unicode set elements */
  def decode(elem: Elem): Set[Int] = elem match
    case Elem.Code(code)        => Set(code)
    case Elem.Range(start, end) => (start to end).toSet

  /** JSON decoder for elements */
  given Decoder[Elem] = new Decoder[Elem] {
    final def apply(c: HCursor): Decoder.Result[Elem] = c.value.asArray match
      case Some(Vector(start, end)) =>
        for {
          s <- start.as[Int]
          e <- end.as[Int]
        } yield Elem.Range(s, e)
      case _ => for (code <- c.value.as[Int]) yield Elem.Code(code)
  }

  /** JSON encoder for elements */
  given Encoder[Elem] = new Encoder[Elem] {
    final def apply(elem: Elem): Json = elem match
      case Elem.Code(code) => Json.fromInt(code)
      case Elem.Range(start, end) =>
        Json.fromValues(List(Json.fromInt(start), Json.fromInt(end)))
  }
}
