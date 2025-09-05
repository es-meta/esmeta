package esmeta.spec

import esmeta.LINE_SEP
import esmeta.lang.*
import esmeta.lang.util.*
import esmeta.spec.util.*
import org.jsoup.nodes.Element

/** abstract algorithms in ECMA-262 */
case class Algorithm(
  head: Head,
  body: Step,
  code: String,
) extends SpecElem {

  /** HTML elements */
  var elem: Element = Element("emu-alg")

  /** check whether it is incomplete */
  lazy val complete: Boolean = incompleteSteps.isEmpty

  /** algorithm name */
  lazy val name: String = head.fname

  /** normalized algorithm name */
  lazy val normalizedName: String = name.replace("/", "").replace("`", "")

  /** return types */
  lazy val retTy: Type = head.retTy

  /** get all steps */
  lazy val steps: List[Step] = StepCollector(body)

  /** get incomplete algorithm steps */
  lazy val incompleteSteps: List[Step] =
    steps.filter(!_.complete)

  /** get complete algorithm steps */
  lazy val completeSteps: List[Step] =
    steps.filter(_.complete)

  /** normalized code */
  lazy val normalizedCode = Algorithm.normalizeCode(code)

  /** parsed result equals with spec before parsing */
  lazy val equals = normalizedCode == body.toString

  lazy val lineDiff =
    val codeList = normalizedCode.split("\n")
    val bodyList = body.toString.split("\n")

    codeList.zip(bodyList).filter(_ != _)

  lazy val lineDiffStr = lineDiff
    .map((l, r) => s"spec\t>> $l\nparsed\t<< $r")
    .mkString("\n\n")
}

object Algorithm {

  /** normalize code by removing unnecessary indents and trailing spaces */
  def normalizeCode(code: String): String = {
    // find the indentation level
    val level = code.linesIterator
      .filter(_.nonEmpty)
      .map(_.takeWhile(_.isWhitespace).length / 2)
      .nextOption
      .getOrElse(0)
    val numDrops = (level - 1) * 2
    // normalize the code
    code.linesIterator
      .map(_.drop(numDrops))
      .mkString(LINE_SEP)
      .replaceAll("\\s+$", "") // remove trailing spaces
  }
}
