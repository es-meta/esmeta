package esmeta.extractor

import esmeta.lang.{util => LangUtil}
import esmeta.spec.{Dfn, Table}
import esmeta.ty.{TyDecl, TyModel, ValueTy}
import esmeta.util.BasicParsers
import esmeta.util.BaseUtils.*
import esmeta.util.ManualInfo

/** type model extractor from field tables and glossary definitions
  *
  * A field table is a table whose first column is `Field Name` or `Internal
  * Slot`. Its caption determines the type name, and its value column (`Value`,
  * `Value Type`, or `Type`) determines the field types. Extracted declarations
  * are overridden by the manual type model.
  */
class TyModelExtractor(
  tables: Map[String, Table],
  dfns: List[Dfn],
  parser: LangUtil.Parsers,
) extends BasicParsers {

  /** extracted type model merged with the manual type model */
  lazy val result: TyModel = TyModel(decls) merge ManualInfo.tyModel

  /** type declarations extracted from field tables */
  lazy val decls: List[TyDecl] = for {
    table <- tables.values.toList.sortBy(_.id)
    if isFieldTable(table)
    name <- getName(table)
    idx <- getValueIdx(table)
    fields = table.rows.flatMap(getField(_, idx))
    if fields.nonEmpty
  } yield TyDecl(name, None, fields)

  // ---------------------------------------------------------------------------
  // field tables
  // ---------------------------------------------------------------------------
  private val FIELD_HEADERS = Set("Field Name", "Internal Slot")
  private val VALUE_HEADERS = Set("Value", "Value Type", "Type")

  private def isFieldTable(table: Table): Boolean =
    table.header.headOption.exists(FIELD_HEADERS)

  private def getValueIdx(table: Table): Option[Int] =
    table.header.indexWhere(VALUE_HEADERS) match
      case -1  => warnNone(s"no value column: ${table.caption}")
      case idx => Some(idx)

  // a row `[[name]] | ... | <type description> | ...` to a field declaration
  private val fieldName = "\\[\\[(\\w+)\\]\\]".r
  private def getField(cells: List[String], idx: Int): Option[TyDecl.Elem] =
    cells match
      case fieldName(name) :: _ =>
        val ty = cells.lift(idx).fold("Any")(getTyStr)
        Some(TyDecl.Elem.Field(name, false, ty))
      case _ => None

  // a natural-language type description to a type string (`Any` if unknown)
  private def getTyStr(desc: String): String = {
    for {
      ty <- optional(parser.parseBy(parser.langTy)(desc.trim))
      str = ty.toString
      _ <- optional(ValueTy.from(str)) // must be a valid type string
    } yield str
  }.getOrElse("Any")

  // ---------------------------------------------------------------------------
  // type names
  // ---------------------------------------------------------------------------
  // a type name from a table caption, canonicalized by glossary definitions
  private def getName(table: Table): Option[String] =
    optional(parseBy(caption)(table.caption))
      .map(canonicalize)
      .orElse(warnNone(s"unknown field table caption: ${table.caption}"))

  private lazy val caption: Parser[String] =
    "Additional Fields of" ~> name |
    "Internal Slots of" ~> name |
    name <~ "Fields"

  // a type name: capitalized words, singularized (`Instances` are dropped)
  private lazy val name: Parser[String] =
    lazy val plural =
      "Instances" ^^^ "" | "Records" ^^^ "Record" | "Objects" ^^^ "Object"
    lazy val word = not("Fields" | plural) ~> "[A-Za-z]+(-[A-Za-z]+)*".r
    rep1(word) ~ opt(plural) ^^ {
      case ws ~ p => normalize((ws ++ p).mkString(" "))
    }

  private lazy val dfnNames: Set[String] = (for {
    dfn <- dfns if dfn.name.headOption.exists(_.isUpper)
    form <- dfn.forms
  } yield normalize(form)).toSet
  private def canonicalize(name: String): String =
    (name :: List("Record", "Event").map(name.stripSuffix))
      .find(dfnNames)
      .getOrElse(name)

  // `Async-from-Sync Iterator` -> `AsyncFromSyncIterator`
  private def normalize(name: String): String =
    name.trim.split("[\\s-]+").map(_.capitalize).mkString

  private def warnNone(msg: String): None.type = { warn(msg); None }
}
