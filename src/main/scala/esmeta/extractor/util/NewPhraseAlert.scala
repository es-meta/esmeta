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
        interval = (line until endLine + 1).toSet
        if (interval intersect diffLines).nonEmpty
      } do
        GitHubAction.println(
          tag = "warning",
          file = Some("spec.html"),
          line = Some(line),
          endLine = Some(endLine),
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
        loc = langType.loc.getOrElse(???)
        line = headElem.startLine + loc.start.line - 1
        endLine = headElem.startLine + loc.end.line - 1
        interval = (line until endLine + 1).toSet
        if (interval intersect diffLines).nonEmpty
      } do
        GitHubAction.println(
          tag = "warning",
          file = Some("spec.html"),
          title = Some("Newly Introduced Unkown Type"),
          line = Some(line),
          endLine = Some(endLine),
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

  // NOTE that column numbers might not be accurate
  extension (elem: Element)

    /* start line number of the element (1-base) */
    def startLine: Int = elem.sourceRange.start.lineNumber

    /* end line number of the element (1-base) */
    def endLine: Int = elem.sourceRange.end.lineNumber
