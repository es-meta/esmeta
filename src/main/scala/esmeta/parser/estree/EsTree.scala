package esmeta.parser.estree

import esmeta.error.EsTreeError
import io.circe.*

/** ESTree nodes, backed by the JSON emitted by the ESTree parser
  *
  * Only the fields required to reconstruct the ECMA-262 concrete syntax tree
  * are accessed, so a thin JSON view is used instead of a full data model of
  * every ESTree node type.
  */
final case class EsTree(json: Json) {

  /** the `type` field, e.g. `Identifier` */
  def tpe: String = str("type")

  /** the source range of this node in UTF-16 code units */
  def start: Int = int("start")
  def end: Int = int("end")

  /** existence check of a non-null field */
  def has(name: String): Boolean = field(name).isDefined

  /** string fields */
  def str(name: String): String =
    strOpt(name).getOrElse(fail(s"no string field `$name`"))
  def strOpt(name: String): Option[String] = field(name).flatMap(_.asString)

  /** boolean fields (absent means `false`) */
  def bool(name: String): Boolean =
    field(name).flatMap(_.asBoolean).getOrElse(false)

  /** integer fields */
  def int(name: String): Int =
    field(name).flatMap(_.asNumber).flatMap(_.toInt) match
      case Some(n) => n
      case None    => fail(s"no integer field `$name`")

  /** node fields */
  def apply(name: String): EsTree =
    get(name).getOrElse(fail(s"no node field `$name`"))
  def get(name: String): Option[EsTree] = field(name).map(EsTree(_))

  /** node list fields, where `null` elements (array holes) become `None` */
  def items(name: String): List[Option[EsTree]] =
    field(name).flatMap(_.asArray) match
      case Some(vs) =>
        vs.toList.map(v => if (v.isNull) None else Some(EsTree(v)))
      case None => fail(s"no list field `$name`")

  /** node list fields without holes */
  def list(name: String): List[EsTree] =
    items(name).map(_.getOrElse(fail(s"unexpected hole in `$name`")))

  /** the raw JSON of this node, for error messages */
  override def toString: String = json.noSpaces

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  private def field(name: String): Option[Json] =
    json.asObject.flatMap(_(name)).filter(!_.isNull)

  private def fail(msg: String): Nothing =
    throw EsTreeError(s"$msg in ${json.noSpaces.take(200)}")
}
