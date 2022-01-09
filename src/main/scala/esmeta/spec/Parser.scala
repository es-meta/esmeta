package esmeta.spec

import esmeta.LINE_SEP
import esmeta.spec.parsers.*
import esmeta.util.HtmlUtils.*
import esmeta.util.BaseUtils.*
import org.jsoup.nodes.Document
import scala.util.parsing.combinator.*

/** specification parser */
object Parser extends ProductionParsers {

  /** parses a specification */
  def parseSpec(content: String): Spec = {
    val document = parseHtml(content)
    Spec(
      version = None,
      grammar = parseGrammar(document),
      algorithms = parseAlgorithms(document),
    )
  }

  /** parses a grammar */
  def parseGrammar(document: Document): Grammar = {
    val elems =
      getElems(document, "emu-grammar[type=definition]:not([example])")
    val prods = for {
      elem <- elems
      content = unescapeHtml(elem.html.trim)
      prods = parseProductions(content)
      prod <- prods
    } yield prod
    Grammar(prods)
  }

  /** parses productions */
  def parseProductions(content: String): List[Production] =
    parseAll(prods, content).get

  /** parses algorithms */
  def parseAlgorithms(document: Document): List[Algorithm] = ???

  /** parses an algorithm */
  def parseAlgorithm(content: String): Algorithm = ???

  /** parses an algorithm head */
  def parseHead(content: String): Head = ???
}
