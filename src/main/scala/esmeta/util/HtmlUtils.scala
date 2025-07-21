package esmeta.util

import esmeta.*
import org.apache.commons.text.StringEscapeUtils
import org.jsoup.*
import org.jsoup.nodes.*
import org.jsoup.parser.*
import org.jsoup.select.*

/** HTML utilities */
object HtmlUtils {

  /** extensions for strings */
  extension (str: String) {

    /** revert entity name to character */
    def unescapeHtml: String = StringEscapeUtils.unescapeHtml4(str)

    /** revert character to entity name */
    def escapeHtml: String = StringEscapeUtils.escapeHtml4(str)

    /** escape ECMAScript file to pass it to shell */
    def escapeES: String = StringEscapeUtils.escapeXSI(str)

    /** parse HTML string */
    def toHtml: Document =
      val parser = Parser.htmlParser().setTrackPosition(true)
      val document = Jsoup.parse(str, parser)
      document.outputSettings.prettyPrint(false)
      document
  }

  /** extensions for Elements */
  extension (elems: Elements) {

    /** convert Elements to a list of Element */
    def toList: List[Element] =
      elems.toArray(Array[Element]()).toList
  }

  /** extensions for Element */
  extension (elem: Element) {

    /** get Element array using queries */
    def getElems(query: String): List[Element] =
      elem.select(query).toList

    /** get children of an element */
    def getChildren: List[Element] = elem.children.toList

    /** get content of an element */
    def getContent: String = elem.html.trim.unescapeHtml

    /** get content of an element */
    def getText: String = elem.text.trim.unescapeHtml

    /** get first child element */
    def getFirstChildElem: Element = elem.child(0)

    /** get first child content */
    def getFirstChildContent: String = elem.child(0).getContent

    /** get first sibling element */
    def getFirstSiblingElem: Element = elem.siblingElements.get(0)

    /** get first sibling content */
    def getFirstSiblingContent: String =
      elem.getFirstSiblingElem.getContent

    /** get previous sibling element */
    def getPrevElem: Element = elem.previousElementSibling

    /** get previous sibling content */
    def getPrevContent: String = elem.getPrevElem.getContent

    /** get previous sibling text */
    def getPrevText: String = elem.getPrevElem.getText

    /** convert an element to a data map */
    def toDataMap: Map[String, String] = {
      var map = Map[String, String]()
      var key = ""
      for (child <- elem.getChildren) child.tagName match {
        case "dt" => key = child.text
        case "dd" => map += key -> child.text
      }
      map
    }

    /** get id of the container */
    def getId: String = {
      if (elem.id != "") elem.id
      else if (elem.parent == null) ""
      else elem.parent.getId
    }

    /** get ids of all containers */
    def getIds: List[String] = {
      val ids = if (elem.parent == null) Nil else elem.parent.getIds
      if (elem.id == "") ids else elem.id :: ids
    }
  }
}
