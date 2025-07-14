package esmeta.spec.util

import esmeta.*
import esmeta.spec.Spec
import esmeta.util.ManualInfo
import esmeta.util.SystemUtils.dumpFile
import io.circe.*, io.circe.syntax.*, io.circe.parser.*
import org.jsoup.nodes.Element

/* GitHub action to warn novel yet steps */
object GitHubAction:

  extension (elem: Element)
    def startLine: Int = elem.sourceRange.start.lineNumber
    def startCol: Int = elem.sourceRange.start.columnNumber
    def endLine: Int = elem.sourceRange.end.lineNumber
    def endCol: Int = elem.sourceRange.end.columnNumber

  def report(
    spec: Spec,
  ): Unit = {
    // val reportPath = sys.env.get("GITHUB_STEP_SUMMARY")
    // reportPath match
    //   case None =>
    //     printWorkflow(
    //       tag = "error",
    //       title = Some("Report Path Not Found"),
    //       message = Some("cannot find GITHUB_STEP_SUMMARY"),
    //     )
    //   case Some(path) =>
    //     dumpFile(
    //       """
    //       |# ESMeta Unknown-Phrase Report
    //       |
    //       |## Unknown Steps
    //       |""".stripMargin ++
    //       spec.incompleteSteps
    //         .map(step =>
    //           s"  * `${step.toString(detail = true, location = false).asJson.noSpaces}`",
    //         )
    //         .mkString("\n"),
    //       reportPath.get,
    //       append = true,
    //     )
  }

  def warnYets(
    spec: Spec,
    ignoreYetSteps: List[String],
    ignoreYetTypes: List[String],
  ): Unit =
    for {
      algo <- spec.algorithms
      elem = algo.elem
    } do {

      val acceptables = ManualInfo.compileRule("inst").keySet ++ ignoreYetSteps

      // warn yet steps
      for {
        step <- algo.steps
        if (step.isInstanceOf[lang.YetStep])
        if (!acceptables.contains(
          step.toString(detail = true, location = false),
        ))
      } do
        printWorkflow(
          tag = "warning",
          filename = Some("spec.html"),
          // -1, cause both 1-based
          line = Some(
            elem.startLine + step.loc.getOrElse(???).start.line - 1,
          ),
          endLine = Some(
            elem.startLine + step.loc.getOrElse(???).end.line - 1,
          ),
          // col = Some(elem.startCol + step.loc.getOrElse(???).start.column - 1),
          // endColumn =
          //   Some(elem.startCol + step.loc.getOrElse(???).end.column - 1),
          title = Some("Newly Introduced Unkown Step"), // TODO
          // TODO
          message = Some(
            s"""
        | This step cannot be understand by ESMeta.
        | Type checking body of this algorithm (${algo.name}) will not be performed after this line.""".stripMargin
              .replace('\n', ' ')
              .trim(),
          ), // TODO
        )

      for {
        langType <- (algo.retTy :: algo.head.funcParams.map(_.ty))
        headElem = algo.headElem
        if (langType.ty.isInstanceOf[ty.UnknownTy] &&
        langType.ty.asInstanceOf[ty.UnknownTy].msg.isDefined)
        if (!ignoreYetTypes.contains(langType.toString))
      } do
        printWorkflow(
          tag = "warning",
          filename = Some("spec.html"),
          // -1, cause both 1-based
          line = Some(
            headElem.startLine + langType.loc.getOrElse(???).start.line, // - 1,
          ),
          endLine = Some(
            headElem.startLine + langType.loc.getOrElse(???).end.line, // - 1
          ),
          title = Some("Newly Introduced Unkown Type"), // TODO
          // TODO
          message = Some(
            s"""
        | This is a type which ESMeta cannot understand. This will cause following consequences:
        | 1. This algorithm (${algo.name}) will not be in the initial type check targets.
        | 2. This algorithm can be type checked when called by another algorithm in the working set, but type (${langType}) will be treated as bottom (⊥).
        |""".stripMargin
              .replace('\n', ' ')
              .trim(),
          ), // TODO
        )
    }

  /** print workflow command for GitHub Actions
    *
    * @param filename
    * @param line
    *   1-based
    * @param endLine
    */
  private def printWorkflow(
    tag: "warning" | "error",
    filename: Option[String] = None,
    line: Option[Int] = None,
    endLine: Option[Int] = None,
    col: Option[Int] = None,
    endColumn: Option[Int] = None,
    title: Option[String] = None,
    message: Option[String] = None,
  ): Unit =

    val args = List(
      "file" -> filename,
      "line" -> line,
      "endLine" -> endLine,
      "col" -> col,
      "endColumn" -> endColumn,
      "title" -> title,
    ).flatMap { case (param, opt) => opt.map(param -> _) }
      .map { case (param, value) => s"$param=$value" }
      .mkString(",")

    println(s"::${tag} ${args}::${message.getOrElse("")}")
