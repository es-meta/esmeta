package esmeta.spec

import esmeta.LINE_SEP
import esmeta.lang.Block
import esmeta.spec.Utils.{given, *}
import esmeta.util.HtmlUtils.*
import esmeta.util.SystemUtils.concurrent
import org.jsoup.nodes.*

/** specification parser */
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
  ): List[Algorithm] = concurrent(for {
    elem <- document.getElems("emu-alg:not([example])")
  } yield () => parseAlgorithms(elem, idxMap)).toList.flatten

  /** parses an algorithm */
  def parseAlgorithms(
    elem: Element,
    idxMap: Map[String, (Int, Int)],
  ): List[Algorithm] = for {
    head <- parseHeads(elem, idxMap)
    id = elem.getId
    code = elem.html.unescapeHtml
    body = Block.from(code)
  } yield Algorithm(head, id, body, code)

  /** TODO ignores elements whose parents' ids are in this list */
  val IGNORE_ALGO_PARENT_IDS = Set(
    // TODO filter algorithms for example or shorthands
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
  )

  /** parses algorithm heads */
  def parseHeads(elem: Element, idxMap: Map[String, (Int, Int)]): List[Head] = {
    var parent = elem.parent
    if (
      (parent.id endsWith "statement-rules") ||
      (parent.id endsWith "expression-rules")
    ) parent = parent.parent

    if (IGNORE_ALGO_PARENT_IDS contains parent.id) return Nil
    if (parent.tagName != "emu-clause") return Nil
    if (elem.isNotation) return Nil

    parent.attr("type") match {
      case "abstract operation" =>
        parseAbsOpHead(parent, elem, false)
      case "host-defined abstract operation" =>
        parseAbsOpHead(parent, elem, true)
      case "numeric method" =>
        parseNumMethodHead(parent, elem)
      case "sdo" =>
        parseSdoHead(parent, elem, idxMap)
      case "concrete method" =>
        parseConcMethodHead(parent, elem)
      case "internal method" =>
        parseInMethodHead(parent, elem)
      case _ =>
        parseBuiltinHead(parent, elem)
    }
  }

  // ///////////////////////////////////////////////////////////////////////////
  // Private Helpers
  // ///////////////////////////////////////////////////////////////////////////
  // get abstract operation heads
  private def parseAbsOpHead(
    parent: Element,
    elem: Element,
    isHostDefined: Boolean,
  ): List[AbstractOperationHead] =
    val headContent = parent.getFirstChildContent
    val generator = parseBy(absOpHeadGen)(headContent)
    List(generator(isHostDefined))

  // get numeric method heads
  private def parseNumMethodHead(
    parent: Element,
    elem: Element,
  ): List[NumericMethodHead] =
    val headContent = parent.getFirstChildContent
    List(parseBy(numMethodHead)(headContent))

  // get syntax-directed operation (SDO) heads
  private def parseSdoHead(
    parent: Element,
    elem: Element,
    idxMap: Map[String, (Int, Int)],
  ): List[SyntaxDirectedOperationHead] = {
    val headContent = parent.getFirstChildContent
    val prevContent = elem.getPrevContent
    val defaultCaseStr =
      "Every grammar production alternative in this specification which is not listed below implicitly has the following default definition of"
    val generator = parseBy(sdoHeadGen)(headContent)
    // to hande "default" case algorithms
    if (!prevContent.startsWith(defaultCaseStr)) {
      // normal case
      for {
        prod <- parse[List[Production]](prevContent)
        lhsName = prod.lhs.name
        rhs <- prod.rhsList
        rhsName <- rhs.allNames
        syntax = lhsName + ":" + rhsName
        (idx, subIdx) = idxMap(syntax)
        rhsParams = rhs.getRhsParams
        target = SyntaxDirectedOperationHead.Target(
          lhsName,
          idx,
          subIdx,
          rhsParams,
        )
      } yield generator(Some(target))
    } else {
      // special 'Default' case: assigned to special LHS named "Default")
      List(generator(None))
    }
  }

  // get concrete method heads
  private def parseConcMethodHead(
    parent: Element,
    elem: Element,
  ): List[ConcreteMethodHead] =
    val headContent = parent.getFirstChildContent
    val generator = parseBy(concMethodHeadGen)(headContent)
    val dataMap = elem.getPrevElem.toDataMap
    val forData = dataMap("for")
    val receiverParam = parseBy(paramDesc)(forData)
    List(generator(receiverParam))

  // get internal method heads
  private def parseInMethodHead(
    parent: Element,
    elem: Element,
  ): List[InternalMethodHead] =
    val headContent = parent.getFirstChildContent
    val generator = parseBy(inMethodHeadGen)(headContent)
    val dataMap = elem.getPrevElem.toDataMap
    val forData = dataMap("for")
    val receiverParam = parseBy(paramDesc)(forData)
    List(generator(receiverParam))

  // get built-in heads
  private def parseBuiltinHead(
    parent: Element,
    elem: Element,
  ): List[BuiltinHead] =
    val headContent = parent.getFirstChildContent
    List(parseBy(builtinHead)(headContent))
}
