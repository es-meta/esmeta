package esmeta.spec

import esmeta.LINE_SEP
import esmeta.spec.Utils.*
import esmeta.spec.parsers.*
import esmeta.util.HtmlUtils.*
import esmeta.util.BaseUtils.*
import org.jsoup.nodes.*
import scala.util.parsing.combinator.*

/** specification parser */
object Parser {
  val parser = new ProductionParsers with HeadParsers

  /** parses a specification */
  def parseSpec(content: String): Spec = {
    val document = parseHtml(content)
    val grammar = parseGrammar(document)
    val algorithms = parseAlgorithms(document, getIdxMap(grammar))
    Spec(
      version = None,
      grammar = grammar,
      algorithms = algorithms,
    )
  }

  /** parses a grammar */
  def parseGrammar(document: Document): Grammar = {
    import Utils.*
    val allProds = for {
      elem <- getElems(document, "emu-grammar[type=definition]:not([example])")
      content = unescapeHtml(elem.html.trim)
      prods = parseProductions(content)
      prod <- prods
      inAnnex = isInAnnex(elem)
    } yield (prod, inAnnex)
    val prods = sort(for ((prod, inAnnex) <- allProds if !inAnnex) yield prod)
    val prodsForWeb = sort(for (
      (prod, inAnnex) <- allProds if inAnnex
    ) yield prod)
    Grammar(prods, prodsForWeb)
  }

  /** parses productions */
  def parseProductions(content: String): List[Production] =
    parse(parser.prods, content)

  /** parses algorithms */
  def parseAlgorithms(
    document: Document,
    idxMap: Map[String, (Int, Int)],
  ): List[Algorithm] =
    for {
      elem <- getElems(document, "emu-alg:not([example])")
      algo <- parseAlgorithms(elem, idxMap)
    } yield algo

  /** parses an algorithm */
  def parseAlgorithms(
    elem: Element,
    idxMap: Map[String, (Int, Int)],
  ): List[Algorithm] = for {
    head <- parseHeads(elem, idxMap)
  } yield ???

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
    // TODO handle untyped SDO
    "sec-identifiers-runtime-semantics-evaluation",
    "sec-this-keyword-runtime-semantics-evaluation",
    "sec-literals-runtime-semantics-evaluation",
    "sec-array-initializer-runtime-semantics-evaluation",
    "sec-object-initializer-runtime-semantics-evaluation",
    "sec-regular-expression-literals-runtime-semantics-evaluation",
    "sec-template-literals-runtime-semantics-evaluation",
    "sec-grouping-operator-runtime-semantics-evaluation",
    "sec-property-accessors-runtime-semantics-evaluation",
    "sec-new-operator-runtime-semantics-evaluation",
    "sec-function-calls-runtime-semantics-evaluation",
    "sec-super-keyword-runtime-semantics-evaluation",
    "sec-optional-chaining-evaluation",
    "sec-import-call-runtime-semantics-evaluation",
    "sec-tagged-templates-runtime-semantics-evaluation",
    "sec-meta-properties-runtime-semantics-evaluation",
    "sec-postfix-increment-operator-runtime-semantics-evaluation",
    "sec-prefix-increment-operator-runtime-semantics-evaluation",
    "sec-delete-operator-runtime-semantics-evaluation",
    "sec-void-operator-runtime-semantics-evaluation",
    "sec-typeof-operator-runtime-semantics-evaluation",
    "sec-%foriniteratorprototype%.next",
    "sec-addition-operator-plus-runtime-semantics-evaluation",
    "sec-arrow-function-definitions-runtime-semantics-evaluation",
    "sec-assignment-operators-runtime-semantics-evaluation",
    "sec-async-arrow-function-definitions-runtime-semantics-evaluation'",
    "sec-async-function-definitions-runtime-semantics-evaluation",
    "sec-asyncgenerator-definitions-evaluation",
    "sec-binary-bitwise-operators-runtime-semantics-evaluation",
    "sec-binary-logical-operators-runtime-semantics-evaluation",
    "sec-bitwise-not-operator-runtime-semantics-evaluation",
    "sec-block-runtime-semantics-evaluation",
    "sec-break-statement-runtime-semantics-evaluation",
    "sec-class-definitions-runtime-semantics-evaluation",
    "sec-comma-operator-runtime-semantics-evaluation",
    "sec-conditional-operator-runtime-semantics-evaluation",
    "sec-continue-statement-runtime-semantics-evaluation",
    "sec-debugger-statement-runtime-semantics-evaluation",
    "sec-empty-statement-runtime-semantics-evaluation",
    "sec-equality-operators-runtime-semantics-evaluation",
    "sec-exp-operator-runtime-semantics-evaluation",
    "sec-exports-runtime-semantics-evaluation",
    "sec-expression-statement-runtime-semantics-evaluation",
    "sec-for-in-and-for-of-statements-runtime-semantics-evaluation",
    "sec-function-definitions-runtime-semantics-evaluation",
    "sec-generator-function-definitions-runtime-semantics-evaluation'",
    "sec-if-statement-runtime-semantics-evaluation",
    "sec-labelled-statements-runtime-semantics-evaluation",
    "sec-left-shift-operator-runtime-semantics-evaluation",
    "sec-let-and-const-declarations-runtime-semantics-evaluation",
    "sec-logical-not-operator-runtime-semantics-evaluation",
    "sec-module-semantics-runtime-semantics-evaluation",
    "sec-multiplicative-operators-runtime-semantics-evaluation",
    "sec-relational-operators-runtime-semantics-evaluation",
    "sec-return-statement-runtime-semantics-evaluation",
    "sec-script-semantics-runtime-semantics-evaluation",
    "sec-signed-right-shift-operator-runtime-semantics-evaluation",
    "sec-statement-semantics-runtime-semantics-evaluation",
    "sec-subtraction-operator-minus-runtime-semantics-evaluation",
    "sec-switch-statement-runtime-semantics-evaluation",
    "sec-throw-statement-runtime-semantics-evaluation",
    "sec-try-statement-runtime-semantics-evaluation",
    "sec-unary-minus-operator-runtime-semantics-evaluation",
    "sec-unary-plus-operator-runtime-semantics-evaluation",
    "sec-unsigned-right-shift-operator-runtime-semantics-evaluation'",
    "sec-variable-statement-runtime-semantics-evaluation",
    "sec-with-statement-runtime-semantics-evaluation",
    "sec-async-arrow-function-definitions-runtime-semantics-evaluation",
    "sec-generator-function-definitions-runtime-semantics-evaluation",
    "sec-postfix-decrement-operator-runtime-semantics-evaluation",
    "sec-prefix-decrement-operator-runtime-semantics-evaluation",
    "sec-unsigned-right-shift-operator-runtime-semantics-evaluation",
    // TODO handle unusual html structure of HasCallInTailPosition
    "sec-statement-rules",
    "sec-expression-rules",
    // TODO handle default cases
    "sec-static-semantics-contains",
    "sec-static-semantics-allprivateidentifiersvalid",
    "sec-static-semantics-containsarguments",
    // TODO old optional/variadic parameters in built-in functions
    "sec-object-value",
    "sec-object.assign",
    "sec-object.prototype.tolocalestring",
    "sec-function.prototype.bind",
    "sec-function.prototype.call",
    "sec-symbol-description",
    "sec-error-message",
    "sec-nativeerror",
    "sec-aggregate-error",
    "sec-number.prototype.tostring",
    "sec-bigint.prototype.tostring",
    "sec-math.hypot",
    "sec-math.max",
    "sec-math.min",
    "sec-date",
    "sec-date.utc",
    "sec-date.prototype.setfullyear",
    "sec-date.prototype.sethours",
    "sec-date.prototype.setminutes",
    "sec-date.prototype.setmonth",
    "sec-date.prototype.setseconds",
    "sec-date.prototype.setutcfullyear",
    "sec-date.prototype.setutchours",
    "sec-date.prototype.setutcminutes",
    "sec-date.prototype.setutcmonth",
    "sec-date.prototype.setutcseconds",
    "sec-string.fromcharcode",
    "sec-string.fromcodepoint",
    "sec-string.raw",
    "sec-string.prototype.concat",
    "sec-string.prototype.endswith",
    "sec-string.prototype.includes",
    "sec-string.prototype.indexof",
    "sec-string.prototype.lastindexof",
    "sec-string.prototype.localecompare",
    "sec-string.prototype.normalize",
    "sec-string.prototype.padend",
    "sec-string.prototype.padstart",
    "sec-string.prototype.startswith",
    "sec-array",
    "sec-array.from",
    "sec-array.of",
    "sec-array.prototype.concat",
    "sec-array.prototype.copywithin",
    "sec-array.prototype.every",
    "sec-array.prototype.fill",
    "sec-array.prototype.filter",
    "sec-array.prototype.find",
    "sec-array.prototype.findindex",
    "sec-array.prototype.flat",
    "sec-array.prototype.flatmap",
    "sec-array.prototype.foreach",
    "sec-array.prototype.includes",
    "sec-array.prototype.indexof",
    "sec-array.prototype.lastindexof",
    "sec-array.prototype.map",
    "sec-array.prototype.push",
    "sec-array.prototype.reduce",
    "sec-array.prototype.reduceright",
    "sec-array.prototype.some",
    "sec-array.prototype.splice",
    "sec-array.prototype.tolocalestring",
    "sec-array.prototype.unshift",
    "sec-%typedarray%.from",
    "sec-%typedarray%.of",
    "sec-%typedarray%.prototype.copywithin",
    "sec-%typedarray%.prototype.every",
    "sec-%typedarray%.prototype.fill",
    "sec-%typedarray%.prototype.filter",
    "sec-%typedarray%.prototype.find",
    "sec-%typedarray%.prototype.findindex",
    "sec-%typedarray%.prototype.foreach",
    "sec-%typedarray%.prototype.includes",
    "sec-%typedarray%.prototype.indexof",
    "sec-%typedarray%.prototype.lastindexof",
    "sec-%typedarray%.prototype.map",
    "sec-%typedarray%.prototype.reduce",
    "sec-%typedarray%.prototype.reduceright",
    "sec-%typedarray%.prototype.set",
    "sec-%typedarray%.prototype.some",
    "sec-typedarray",
    "sec-map-iterable",
    "sec-map.prototype.foreach",
    "sec-set-iterable",
    "sec-set.prototype.foreach",
    "sec-weakmap-iterable",
    "sec-weakset-iterable",
    "sec-dataview-buffer-byteoffset-bytelength",
    "sec-dataview.prototype.getbigint64",
    "sec-dataview.prototype.getbiguint64",
    "sec-dataview.prototype.getfloat32",
    "sec-dataview.prototype.getfloat64",
    "sec-dataview.prototype.getint16",
    "sec-dataview.prototype.getint32",
    "sec-dataview.prototype.getuint16",
    "sec-dataview.prototype.getuint32",
    "sec-dataview.prototype.setbigint64",
    "sec-dataview.prototype.setbiguint64",
    "sec-dataview.prototype.setfloat32",
    "sec-dataview.prototype.setfloat64",
    "sec-dataview.prototype.setint16",
    "sec-dataview.prototype.setint32",
    "sec-dataview.prototype.setuint16",
    "sec-dataview.prototype.setuint32",
    "sec-json.parse",
    "sec-json.stringify",
    "sec-finalization-registry.prototype.register",
    "sec-%asyncfromsynciteratorprototype%.next",
    "sec-%asyncfromsynciteratorprototype%.return",
    "sec-%asyncfromsynciteratorprototype%.throw",
    "sec-reflect.construct",
    "sec-reflect.get",
    "sec-reflect.set",
  )

  /** parses algorithm heads */
  def parseHeads(elem: Element, idxMap: Map[String, (Int, Int)]): List[Head] = {
    if (IGNORE_ALGO_PARENT_IDS contains elem.parent.id) return Nil
    if (elem.parent.tagName != "emu-clause") return Nil

    elem.parent.attr("type") match {
      case "abstract operation"              => getAbsOpHead(elem, false)
      case "host-defined abstract operation" => getAbsOpHead(elem, true)
      case "numeric method"                  => getNumMethodHead(elem)
      case "sdo"                             => getSdoHead(elem, idxMap)
      case "concrete method"                 => getConcMethodHead(elem)
      case "internal method"                 => getInMethodHead(elem)
      case _                                 => getBuiltinHead(elem)
    }

    // XXX REMOVE
    Nil
  }

  // ///////////////////////////////////////////////////////////////////////////
  // Private Helpers
  // ///////////////////////////////////////////////////////////////////////////
  import Head.*

  // parse with parsing rules
  private def parse[T](rule: parser.Parser[T], str: String): T =
    parser.parseAll(rule, str).get

  // get abstract operation heads
  private def getAbsOpHead(
    elem: Element,
    isHostDefined: Boolean,
  ): List[AbstractOperationHead] =
    val headContent = getFirstSiblingContent(elem)
    val absOpHeadGen = parse(parser.absOpHeadGen, headContent)
    List(absOpHeadGen(isHostDefined))

  // get numeric method heads
  private def getNumMethodHead(
    elem: Element,
  ): List[NumericMethodHead] =
    val headContent = getFirstSiblingContent(elem)
    List(parse(parser.numMethodHead, headContent))

  // get syntax-directed operation (SDO) heads
  private def getSdoHead(
    elem: Element,
    idxMap: Map[String, (Int, Int)],
  ): List[SyntaxDirectedOperationHead] = {
    val headContent = getFirstSiblingContent(elem)
    val prevContent = getPrevContent(elem)
    val sdoHeadGen = parse(parser.sdoHeadGen, headContent)
    for {
      prod <- parse(parser.prods, prevContent)
      lhsName = prod.lhs.name
      rhs <- prod.rhsList
      rhsName <- allNames(rhs)
      syntax = lhsName + ":" + rhsName
      (idx, subIdx) = idxMap(syntax)
      rhsParams = getRhsParams(rhs)
    } yield sdoHeadGen(
      lhsName,
      idx,
      subIdx,
      rhsParams,
    )
  }

  // get concrete method heads
  private def getConcMethodHead(
    elem: Element,
  ): List[ConcreteMethodHead] =
    val headContent = getFirstSiblingContent(elem)
    val concMethodHeadGen =
      parse(parser.concMethodHeadGen, headContent)
    val dataMap = toDataMap(getPrevElem(elem))
    val forData = dataMap("for")
    val receiverParam = parse(parser.paramDesc, forData)
    List(concMethodHeadGen(receiverParam))

  // get internal method heads
  private def getInMethodHead(elem: Element): List[InternalMethodHead] =
    val headContent = getFirstSiblingContent(elem)
    val inMethodHeadGen =
      parse(parser.inMethodHeadGen, headContent)
    val dataMap = toDataMap(getPrevElem(elem))
    val forData = dataMap("for")
    val receiverParam = parse(parser.paramDesc, forData)
    List(inMethodHeadGen(receiverParam))

  // get built-in heads
  private def getBuiltinHead(elem: Element): List[BuiltinHead] =
    val headContent = getFirstSiblingContent(elem)
    List(parse(parser.builtinHead, headContent))
}
