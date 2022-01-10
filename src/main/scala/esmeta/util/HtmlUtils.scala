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

  /** convert Elements to a list of Element */
  def toList(elems: Elements): List[Element] =
    elems.toArray(Array[Element]()).toList

  /** get Element array using queries */
  def getElems(elem: Element, query: String): List[Element] =
    toList(elem.select(query))

  /** get children of an element */
  def getChildren(elem: Element): List[Element] = toList(elem.children)

  /** get content of an element */
  def getContent(elem: Element): String = unescapeHtml(elem.html.trim)

  /** get first sibling element */
  def getFirstSiblingElem(elem: Element): Element = elem.siblingElements.get(0)

  /** get first sibling content */
  def getFirstSiblingContent(elem: Element): String =
    getContent(getFirstSiblingElem(elem))

  /** get previous sibling element */
  def getPrevElem(elem: Element): Element = elem.previousElementSibling

  /** get previous sibling content */
  def getPrevContent(elem: Element): String = getContent(getPrevElem(elem))

  /** convert an element to a data map */
  def toDataMap(elem: Element): Map[String, String] = {
    var map = Map[String, String]()
    var key = ""
    for (child <- getChildren(elem)) child.tagName match {
      case "dt" => key = child.text
      case "dd" => map += key -> child.text
    }
    map
  }
}
