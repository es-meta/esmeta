package esmeta.parser

import esmeta.*
import esmeta.util.SystemUtils.*
import io.circe.*, io.circe.syntax.*, io.circe.parser.*

object Unicode {
  val RECENT_VERSION = "17.0.0"

  lazy val IDStart = getFromContent(esmeta.BuildInfo.idStart)
  lazy val IDContinue = getFromContent(esmeta.BuildInfo.idContinue)

  /** unicode set getter
    *
    * TODO: utilize npm module `@es-meta/unicode-gen`
    */
  def get(property: String, version: String = RECENT_VERSION): Set[Int] =
    val filename = s"$UNICODE_DIR/${property}_$version.json"
    val elems = readJson[List[Elem]](filename)
    elems.flatMap(decode).toSet

  def getFromContent(content: String): Set[Int] =
    val elems = readJsonContent[List[Elem]](content)
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
