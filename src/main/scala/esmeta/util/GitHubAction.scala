package esmeta.util

object GitHubAction:

  /** workflow command for GitHub Actions
    *
    * @notes
    *   1-based line and column numbers
    */
  def println(
    tag: "warning" | "error",
    file: Option[String] = None,
    line: Option[Int] = None,
    endLine: Option[Int] = None,
    col: Option[Int] = None,
    endColumn: Option[Int] = None,
    title: Option[String] = None,
    message: Option[String] = None,
  ): Unit =
    val args = List(
      "file" -> file,
      "line" -> line,
      "endLine" -> endLine,
      "col" -> col,
      "endColumn" -> endColumn,
      "title" -> title,
    ).flatMap { case (param, opt) => opt.map(param -> _) }
      .map { case (param, value) => s"$param=$value" }
      .mkString(",")
    Predef.println(s"::$tag ${args}::${message.getOrElse("")}")
