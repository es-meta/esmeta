package esmeta.util

import esmeta.*
import org.apache.commons.text.StringEscapeUtils
import org.jsoup.*
import org.jsoup.nodes.*
import org.jsoup.select.*

/** HTML utilities */
object HtmlUtils {

  /** revert entity name to character */
  def unescapeHtml(str: String): String = StringEscapeUtils.unescapeHtml4(str)

  /** revert character to entity name */
  def escapeHtml(str: String): String = StringEscapeUtils.escapeHtml4(str)

  /** escape js file to pass it to shell */
  def escapeJS(str: String): String = StringEscapeUtils.escapeXSI(str)

  /** parse HTML string */
  def parseHtml(content: String): Document =
    val document = Jsoup.parse(content)
    document.outputSettings.prettyPrint(false)
    document

  /** get Element array using queries */
  def getElems(elem: Element, query: String): List[Element] =
    elem.select(query).toArray(Array[Element]()).toList

  /** get range of element */
  def getRange(elem: Element): Option[(Int, Int)] =
    val s = elem.attr("s")
    val e = elem.attr("e")
    if (s == "") None else Some((s.toInt, e.toInt))

  /** get raw body of element */
  def getRawBody(elem: Element)(using lines: Array[String]): Array[String] =
    getRange(elem) match {
      case Some((s, e)) if s + 1 < e => lines.slice(s + 1, e - 1)
      case _ => Array(elem.html.replaceAll(LINE_SEP, " "))
    }
}
