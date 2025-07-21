package esmeta.extractor.util

import esmeta.*
import esmeta.spec.Spec
import esmeta.util.*
import io.circe.*, io.circe.syntax.*, io.circe.parser.*
import org.jsoup.nodes.Element

object NewPhraseAlert:
  def warnYets(
    spec: Spec,
    diffLines: Set[Int],
  ): Unit =
    for {
      algo <- spec.algorithms
      elem = algo.elem
    } do {

      val acceptables =
        ManualInfo.compileRule("inst").keySet // ++ ignoreYetSteps

      for {
        step <- algo.steps
        if (step.isInstanceOf[lang.YetStep])
        if (!acceptables.contains(
          step.toString(detail = true, location = false),
        ))
        loc = step.loc.getOrElse(???)
        line = elem.startLine + loc.start.line - 1
        endLine = elem.startLine + loc.end.line - 1
        if (diffLines.contains(line) || diffLines.contains(endLine))
      } do
        GitHubAction.println(
          tag = "warning",
          file = Some("spec.html"),
          line = Some(line),
          endLine = Some(endLine),
          col = Some(elem.startCol + loc.start.column - 1),
          endColumn = Some(elem.startCol + loc.end.column - 1),
          title = Some("Newly Introduced Unkown Step"), // TODO
          message = Some(
            s"""
        | This step cannot be understand by ESMeta.
        | Type checking body of this algorithm (${algo.name}) will not be performed after this line.""".stripMargin
              .replace('\n', ' ')
              .trim(),
          ),
        )

      for {
        langType <- (algo.retTy :: algo.head.funcParams.map(_.ty))
        headElem = algo.headElem
        if (langType.ty.isInstanceOf[ty.UnknownTy] &&
        langType.ty.asInstanceOf[ty.UnknownTy].msg.isDefined)
        // if (!ignoreYetTypes.contains(langType.toString))
      } do
        val loc = langType.loc.getOrElse(???)
        GitHubAction.println(
          tag = "warning",
          file = Some("spec.html"),
          line = Some(headElem.startLine + loc.start.line),
          endLine = Some(headElem.startLine + loc.end.line),
          title = Some("Newly Introduced Unkown Type"),
          message = Some(
            s"""
        | This is a type which ESMeta cannot understand. This will cause following consequences:
        | 1. This algorithm (${algo.name}) will not be in the initial type check targets.
        | 2. This algorithm can be type checked when called by another algorithm in the working set, but type (${langType}) will be treated as bottom (⊥).
        |""".stripMargin
              .replace('\n', ' ')
              .trim(),
          ),
        )
    }

  extension (elem: Element)
    def startLine: Int = elem.sourceRange.start.lineNumber
    def startCol: Int = elem.sourceRange.start.columnNumber
    def endLine: Int = elem.sourceRange.end.lineNumber
    def endCol: Int = elem.sourceRange.end.columnNumber
