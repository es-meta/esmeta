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
}
