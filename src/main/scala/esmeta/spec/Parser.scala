package esmeta.spec

import esmeta.LINE_SEP
import esmeta.lang.Block
import esmeta.spec.Utils.{given, *}
import esmeta.util.HtmlUtils.*
import esmeta.util.BasicParser
import org.jsoup.nodes.*

/** specification parser */
trait Parser[T] extends BasicParser[T, Parsers] { val parser = Parser }
object Parser extends Parsers {

  /** parses a specification */
  def parseSpec(content: String): Spec = {
    val document = content.toHtml
    val grammar = parseGrammar(document)
    val idxMap = grammar.idxMap(forWeb = false)
    val algorithms = parseAlgorithms(document, idxMap)
    Spec(
      version = None,
      grammar = grammar,
      algorithms = algorithms,
    )
  }

  /** parses a grammar */
  def parseGrammar(document: Document): Grammar = {
    val allProds = for {
      elem <- document.getElems("emu-grammar[type=definition]:not([example])")
      content = elem.html.trim.unescapeHtml
      prods = parseProductions(content)
      prod <- prods
      inAnnex = elem.isInAnnex
    } yield (prod, inAnnex)
    val prods =
      (for ((prod, inAnnex) <- allProds if !inAnnex) yield prod).sorted
    val prodsForWeb =
      (for ((prod, inAnnex) <- allProds if inAnnex) yield prod).sorted
    Grammar(prods, prodsForWeb)
  }

  /** parses productions */
  def parseProductions(content: String): List[Production] = parse(content)

  /** parses algorithms */
  def parseAlgorithms(
    document: Document,
    idxMap: Map[String, (Int, Int)],
  ): List[Algorithm] =
    for {
      elem <- document.getElems("emu-alg:not([example])")
      algo <- parseAlgorithms(elem, idxMap)
    } yield algo

  /** parses an algorithm */
  def parseAlgorithms(
    elem: Element,
    idxMap: Map[String, (Int, Int)],
  ): List[Algorithm] = for {
    head <- parseHeads(elem, idxMap)
    id = elem.getId
    code = elem.html
    body = Block(code)
  } yield Algorithm(head, id, body, code)

  /** TODO ignores elements whose parents' ids are in this list */
  val IGNORE_ALGO_PARENT_IDS = Set(
    // TODO filter algorithms for example or shorthands
    "sec-algorithm-conventions-syntax-directed-operations",
    "sec-implicit-completion-values",
    "sec-throw-an-exception",
    "sec-returnifabrupt",
    "sec-abstract-closure",
    "sec-ifabruptcloseiterator",
    "sec-ifabruptrejectpromise",
    // TODO handle Await
    "await",
    // TODO handle memory model
    "sec-weakref-execution",
    "sec-valid-chosen-reads",
    "sec-coherent-reads",
    "sec-tear-free-aligned-reads",
    "sec-races",
    "sec-data-races",
    // TODO handle unusual header text
    "sec-function-p1-p2-pn-body",
    "sec-generatorfunction",
    "sec-asyncgeneratorfunction",
    "sec-async-function-constructor-arguments",
    // TODO handle unusual html structure of HasCallInTailPosition
    "sec-statement-rules",
    "sec-expression-rules",
    // TODO handle default cases
    "sec-static-semantics-contains",
    "sec-static-semantics-allprivateidentifiersvalid",
    "sec-static-semantics-containsarguments",
  )

  /** parses algorithm heads */
  def parseHeads(elem: Element, idxMap: Map[String, (Int, Int)]): List[Head] = {
    if (IGNORE_ALGO_PARENT_IDS contains elem.parent.id) return Nil
    if (elem.parent.tagName != "emu-clause") return Nil

    elem.parent.attr("type") match {
      case "abstract operation"              => parseAbsOpHead(elem, false)
      case "host-defined abstract operation" => parseAbsOpHead(elem, true)
      case "numeric method"                  => parseNumMethodHead(elem)
      case "sdo"                             => parseSdoHead(elem, idxMap)
      case "concrete method"                 => parseConcMethodHead(elem)
      case "internal method"                 => parseInMethodHead(elem)
      case _                                 => parseBuiltinHead(elem)
    }
  }

  // ///////////////////////////////////////////////////////////////////////////
  // Private Helpers
  // ///////////////////////////////////////////////////////////////////////////
  // get abstract operation heads
  private def parseAbsOpHead(
    elem: Element,
    isHostDefined: Boolean,
  ): List[AbstractOperationHead] =
    val headContent = elem.getFirstSiblingContent
    val generator = parseBy(absOpHeadGen)(headContent)
    List(generator(isHostDefined))

  // get numeric method heads
  private def parseNumMethodHead(
    elem: Element,
  ): List[NumericMethodHead] =
    val headContent = elem.getFirstSiblingContent
    List(parseBy(numMethodHead)(headContent))

  // get syntax-directed operation (SDO) heads
  private def parseSdoHead(
    elem: Element,
    idxMap: Map[String, (Int, Int)],
  ): List[SyntaxDirectedOperationHead] = {
    val headContent = elem.getFirstSiblingContent
    val prevContent = elem.getPrevContent
    val generator = parseBy(sdoHeadGen)(headContent)
    for {
      prod <- parse[List[Production]](prevContent)
      lhsName = prod.lhs.name
      rhs <- prod.rhsList
      rhsName <- rhs.allNames
      syntax = lhsName + ":" + rhsName
      (idx, subIdx) = idxMap(syntax)
      rhsParams = rhs.getRhsParams
    } yield generator(
      lhsName,
      idx,
      subIdx,
      rhsParams,
    )
  }

  // get concrete method heads
  private def parseConcMethodHead(
    elem: Element,
  ): List[ConcreteMethodHead] =
    val headContent = elem.getFirstSiblingContent
    val generator = parseBy(concMethodHeadGen)(headContent)
    val dataMap = elem.getPrevElem.toDataMap
    val forData = dataMap("for")
    val receiverParam = parseBy(paramDesc)(forData)
    List(generator(receiverParam))

  // get internal method heads
  private def parseInMethodHead(elem: Element): List[InternalMethodHead] =
    val headContent = elem.getFirstSiblingContent
    val generator = parseBy(inMethodHeadGen)(headContent)
    val dataMap = elem.getPrevElem.toDataMap
    val forData = dataMap("for")
    val receiverParam = parseBy(paramDesc)(forData)
    List(generator(receiverParam))

  // get built-in heads
  private def parseBuiltinHead(elem: Element): List[BuiltinHead] =
    val headContent = elem.getFirstSiblingContent
    List(parseBy(builtinHead)(headContent))
}
