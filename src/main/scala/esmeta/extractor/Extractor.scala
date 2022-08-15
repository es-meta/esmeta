package esmeta.extractor

import esmeta.*
import esmeta.lang.Step
import esmeta.spec.{*, given}
import esmeta.spec.util.{Parsers => SpecParsers}
import esmeta.util.Git
import esmeta.util.HtmlUtils.*
import esmeta.util.SystemUtils.*
import org.jsoup.nodes.*

/** specification extractor for ECMA-262 */
object Extractor extends SpecParsers {

  /** extracts a specification */
  def apply(
    document: Document,
    version: Option[Git.Version] = None,
  ): Spec =
    val grammar = extractGrammar(document)
    val idxMap = grammar.idxMap
    val algorithms = extractAlgorithms(document, idxMap)
    val tables = extractTables(document)
    Spec(
      version = version,
      grammar = grammar,
      algorithms = algorithms,
      tables = tables,
      typeModel = TypeModel.js, // TODO automatic extraction
      document = document,
    )

  /** extracts a specification with target versions */
  def apply(): Spec =
    apply(readFile(SPEC_HTML).toHtml, Some(Git.currentVersion(ECMA262_DIR)))
  def apply(targetOpt: Option[String]): Spec =
    targetOpt.fold(apply())(apply)
  def apply(target: String): Spec =
    val cur = Git.currentVersion(ECMA262_DIR)
    val version = Git.getVersion(target, ECMA262_DIR)
    Git.changeVersion(version, ECMA262_DIR)
    val document = readFile(SPEC_HTML).toHtml
    Git.changeVersion(cur, ECMA262_DIR)
    apply(document, Some(version))

  /** extracts a grammar */
  def extractGrammar(document: Document): Grammar = {
    val allProds = for {
      elem <- document.getElems("emu-grammar[type=definition]:not([example])")
      content = elem.html.trim.unescapeHtml
      prods = extractProductions(content)
      prod <- prods
      inAnnex = elem.isInAnnex
    } yield (prod, inAnnex)
    val prods =
      (for ((prod, inAnnex) <- allProds if !inAnnex) yield prod).sorted
    val prodsForWeb =
      (for ((prod, inAnnex) <- allProds if inAnnex) yield prod).sorted
    Grammar(prods, prodsForWeb)
  }

  /** extracts productions */
  def extractProductions(content: String): List[Production] = parse(content)

  /** extracts algorithms */
  def extractAlgorithms(
    document: Document,
    idxMap: Map[String, (Int, Int)],
  ): List[Algorithm] =
    // XXX load manually created algorithms
    var manualJobs = for {
      file <- walkTree(MANUALS_DIR) if algoFilter(file.getName)
      content = readFile(file.toString)
      document = content.toHtml
      elem <- document.getElems("emu-alg:not([example])")
    } yield () => extractAlgorithm(elem, idxMap)

    // extract algorithms in spec
    val jobs = for {
      elem <- document.getElems("emu-alg:not([example])")
    } yield () => extractAlgorithm(elem, idxMap)

    concurrent(manualJobs ++ jobs).toList.flatten

  /** extracts an algorithm */
  def extractAlgorithm(
    elem: Element,
    idxMap: Map[String, (Int, Int)],
  ): List[Algorithm] = for {
    head <- extractHeads(elem, idxMap)
    code = elem.html.unescapeHtml
    body = Step.from(code)
  } yield Algorithm(head, elem, body, code)

  /** TODO ignores elements whose parents' ids are in this list */
  val IGNORE_ALGO_PARENT_IDS = Set(
    // TODO handle memory model
    "sec-weakref-execution",
    "sec-valid-chosen-reads",
    "sec-coherent-reads",
    "sec-tear-free-aligned-reads",
    "sec-races",
    "sec-data-races",
  )

  /** extracts algorithm heads */
  def extractHeads(
    elem: Element,
    idxMap: Map[String, (Int, Int)],
  ): List[Head] = {
    var parent = elem.parent
    // TODO more general rules
    if (
      (parent.id endsWith "statement-rules") ||
      (parent.id endsWith "expression-rules")
    ) parent = parent.parent

    // checks whether it is an algorithm that should be ignored
    if (IGNORE_ALGO_PARENT_IDS contains parent.id) return Nil

    // checks whether it is a valid algorithm heaad
    if (parent.tagName != "emu-clause") return Nil

    // consider algorithm head types using `type` attributes
    parent.attr("type") match {
      case "abstract operation" =>
        extractAbsOpHead(parent, elem, false)
      case "host-defined abstract operation" =>
        extractAbsOpHead(parent, elem, true)
      case "numeric method" =>
        extractNumMethodHead(parent, elem)
      case "sdo" =>
        extractSdoHead(parent, elem, idxMap)
      case "concrete method" =>
        extractConcMethodHead(parent, elem)
      case "internal method" =>
        extractInMethodHead(parent, elem)
      case _ =>
        extractUnusualHead(parent, elem)
    }
  }

  /** extracts tables */
  def extractTables(
    document: Document,
  ): Map[String, Table] = (for {
    elem <- document.getElems("emu-table")
    id = elem.getId
    datas = (for {
      row <- elem.getElems("tr")
    } yield row.getChildren.map(_.text)).toList
  } yield id -> Table(id, datas.head, datas.tail)).toMap

  // ---------------------------------------------------------------------------
  // Private Helpers
  // ---------------------------------------------------------------------------
  // get abstract operation heads
  private def extractAbsOpHead(
    parent: Element,
    elem: Element,
    isHostDefined: Boolean,
  ): List[AbstractOperationHead] =
    val headContent = getHeadContent(parent)
    val generator = parseBy(absOpHeadGen)(headContent)
    List(generator(isHostDefined))

  // get numeric method heads
  private def extractNumMethodHead(
    parent: Element,
    elem: Element,
  ): List[NumericMethodHead] =
    val headContent = getHeadContent(parent)
    List(parseBy(numMethodHead)(headContent))

  // get syntax-directed operation (SDO) heads
  private def extractSdoHead(
    parent: Element,
    elem: Element,
    idxMap: Map[String, (Int, Int)],
  ): List[SyntaxDirectedOperationHead] = {
    val headContent = getHeadContent(parent)
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
        rhsParams = rhs.params
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
  private def extractConcMethodHead(
    parent: Element,
    elem: Element,
  ): List[ConcreteMethodHead] =
    val headContent = getHeadContent(parent)
    val generator = parseBy(concMethodHeadGen)(headContent)
    val dataMap = elem.getPrevElem.toDataMap
    val forData = dataMap("for")
    val receiverParam = parseBy(paramDesc)(forData)
    List(generator(receiverParam))

  // get internal method heads
  private def extractInMethodHead(
    parent: Element,
    elem: Element,
  ): List[InternalMethodHead] =
    val headContent = getHeadContent(parent)
    val generator = parseBy(inMethodHeadGen)(headContent)
    val dataMap = elem.getPrevElem.toDataMap
    val forData = dataMap("for")
    val receiverParam = parseBy(paramDesc)(forData)
    List(generator(receiverParam))

  // get built-in heads
  private def extractBuiltinHead(
    parent: Element,
    elem: Element,
  ): List[BuiltinHead] =
    val headContent = getHeadContent(parent)
    List(parseBy(builtinHead)(headContent))

  // handle unusual heads
  private lazy val thisValuePattern =
    "The abstract operation (this\\w+Value) takes argument _(\\w+)_.*".r
  private lazy val aliasPattern =
    "means? the same thing as:".r
  private lazy val anonBuiltinPattern =
    "When a ([A-Za-z.` ]+) is called with argument _(\\w+)_,.*".r
  private def extractUnusualHead(
    parent: Element,
    elem: Element,
  ): List[Head] = elem.getPrevText match
    case thisValuePattern(name, param) =>
      List(AbstractOperationHead(false, name, List(Param(param)), UnknownType))
    case aliasPattern() => extractAbsOpHead(parent, elem, false)
    case anonBuiltinPattern(name, param) =>
      val rname = name.trim.split(" ").map(_.capitalize).mkString
      val ref = BuiltinHead.Ref.YetRef(rname)
      List(BuiltinHead(ref, List(Param(param)), UnknownType))
    case _ if parent.hasAttr("aoid") => Nil
    case _                           => extractBuiltinHead(parent, elem)

  // get head contents from parent elements
  private def getHeadContent(parent: Element): String =
    parent.getFirstChildContent
}
