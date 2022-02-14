package esmeta.spec.util

import esmeta.LINE_SEP
import esmeta.lang.Step
import esmeta.spec.*
import esmeta.util.BasicParsers
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
    val tables = parseTables(document)
    Spec(
      version = None,
      grammar = grammar,
      algorithms = algorithms,
      tables = tables,
      document = document,
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
    code = elem.html.unescapeHtml
    body = Step.from(code)
  } yield Algorithm(head, elem, body, code)

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

  /** parses tables */
  def parseTables(
    document: Document,
  ): List[Table] = for {
    elem <- document.getElems("emu-table")
    id = elem.getId
    datas = (for {
      row <- elem.getElems("tr")
    } yield row.getChildren.map(_.text)).toList
  } yield Table(id, datas.head, datas.tail)

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

/** specification parsers */
trait Parsers extends BasicParsers {
  // skip only white spaces and comments
  protected override val whiteSpace = "[ \t]*//.*|[ \t]+".r

  // production lists
  given prods: Parser[List[Production]] =
    rep1(rep(newline) ~> prod)

  // productions
  given prod: Parser[Production] =
    lhs ~ prodKind ~ opt("one of") ~ rep1(opt(newline) ~> rhs) ^^ {
      case l ~ k ~ Some(_) ~ origRs =>
        val rs = for {
          r <- origRs
          s <- r.symbols
        } yield Rhs(None, List(s), None)
        Production(l, k, true, rs)
      case l ~ k ~ None ~ rs => Production(l, k, false, rs)
    }

  // production kinds
  lazy val prodKind: Parser[Production.Kind] =
    import Production.Kind.*
    ":::" ^^^ NumericString | "::" ^^^ Lexical | ":" ^^^ Syntactic

  // production left-hand-sides (LHSs)
  given lhs: Parser[Lhs] =
    word ~ opt("[" ~> repsep(word, ",") <~ "]") ^^ {
      case name ~ params =>
        Lhs(name, params.getOrElse(Nil))
    }

  // production alternative right-hand-sides (RHSs)
  given rhs: Parser[Rhs] =
    opt(rhsCond) ~ rep1(symbol) ~ opt(rhsId) ^^ {
      case c ~ ss ~ i =>
        Rhs(c, ss, i)
    }

  // RHS conditions
  given rhsCond: Parser[RhsCond] =
    "[" ~> ("[+~]".r) ~ word <~ "]" ^^ {
      case str ~ name =>
        RhsCond(name, str == "+")
    }

  // RHS ids
  lazy val rhsId: Parser[String] = "#" ~> "[-a-zA-Z0-9]+".r

  // grammar symbols
  given symbol: Parser[Symbol] =
    term | butnot | lookahead | butOnlyIf | nt | abbr | unicodeSet | empty | nlt

  // terminals
  lazy val term: Parser[Terminal] =
    "`[^`]+`|```".r ^^ {
      case str =>
        Terminal(str.substring(1, str.length - 1))
    }

  // nonterminals
  lazy val nt: Parser[Nonterminal] =
    word ~ opt("[" ~> rep1sep(ntArg, ",") <~ "]") ~ opt("?") ^^ {
      case name ~ args ~ opt =>
        Nonterminal(name, args.getOrElse(Nil), opt.isDefined)
    }

  // butnot symbols
  lazy val butnot: Parser[ButNot] =
    (nt <~ ("but not" ~ opt("one of"))) ~ rep1sep(symbol, opt("or")) ^^ {
      case base ~ cases => ButNot(base, cases)
    }

  // but-only-if symbols
  // [> but only if MV of |HexDigits| > 0x10FFFF]
  lazy val butOnlyIf: Parser[ButOnlyIf] =
    nt ~ ("[> but only if" ~ opt("the") ~> word <~ "of |" ~ word ~ "|") ~
    "[^\\]]+".r <~ "]" ^^ {
      case base ~ name ~ cond => ButOnlyIf(base, name, cond)
    }

  // empty symbols
  lazy val empty: Parser[Empty.type] = "[empty]" ^^^ Empty

  // no-line-terminator symbols
  lazy val nlt: Parser[NoLineTerminator.type] =
    "\\[no [\\|]?LineTerminator[\\|]? here\\]".r ^^^ NoLineTerminator

  // symbols for code point abbreviations
  lazy val abbr: Parser[CodePointAbbr] =
    "<" ~> word <~ ">" ^^ { CodePointAbbr(_) }

  // symbols for sets of unicode code points with a condition
  lazy val unicodeSet: Parser[UnicodeSet] =
    ">" ~ "any Unicode code point" ~> opt(".+".r) ^^ { UnicodeSet(_) }

  // lookahead symbol
  lazy val lookahead: Parser[Lookahead] =
    "[lookahead " ~> containsSymbol ~ laList <~ "]" ^^ {
      case b ~ cases => Lookahead(b, cases)
    }
  lazy val laList: Parser[List[List[Symbol]]] =
    opt("{") ~> repsep(rep(symbol), ",") <~ opt("}")
  lazy val containsSymbol: Parser[Boolean] =
    ("!=" | "<!" | "∉") ^^^ false | ("==" | "<" | "∈") ^^^ true

  // nonterminal arguments
  given ntArg: Parser[NtArg] =
    import NtArg.Kind.*
    ("+" ^^^ True | "~" ^^^ False | "?" ^^^ Pass) ~ word ^^ {
      case kind ~ name => NtArg(kind, name)
    }

  lazy val id: Parser[String] = "_[^_]+_".r ^^ { s =>
    s.substring(1, s.length - 1)
  }
  lazy val name: Parser[String] = "[a-zA-Z0-9/]+".r
  lazy val headParamType: Parser[String] = "([^_,]|, )+".r
  lazy val refName: Parser[String] = "[_`%a-zA-Z0-9.\\[\\]@ ]+".r

  // runtime/static semantics
  lazy val semanticsKind: Parser[Boolean] =
    (("Runtime" | "Static") <~ "Semantics" ~ ":") ^^ { _ == "Static" }

  // abstract opration (AO) heads
  lazy val absOpHeadGen: Parser[Boolean => AbstractOperationHead] =
    opt(semanticsKind) ~> name ~ params ^^ {
      case name ~ params =>
        (isHostDefined: Boolean) =>
          AbstractOperationHead(name, params, isHostDefined)
    }

  // numeric method heads
  lazy val numMethodHead: Parser[NumericMethodHead] =
    (name <~ "::") ~ name ~ params ^^ {
      case t ~ x ~ ps => NumericMethodHead(t, x, ps)
    }

  // algorithm parameters
  lazy val params: Parser[List[Param]] =
    import Param.Kind.Ellipsis
    opt(
      "(" ~ opt(newline) ~> repsep(param, "," ~ opt(newline)) ~ oldOptParams <~
      opt("," ~ newline) ~ ")",
    ) ^^ {
      case None           => Nil
      case Some(ps ~ ops) => ps ++ ops
    }
  // TODO remove this legacy parser later
  lazy val oldOptParams: Parser[List[Param]] =
    import Param.Kind.*
    "[" ~> opt(",") ~> param ~ opt(oldOptParams) <~ "]" ^^ {
      case p ~ psOpt => p :: psOpt.getOrElse(Nil)
    } | opt(",") ~ "..." ~> param ^^ {
      case p => List(p.copy(kind = Variadic))
    } | success(Nil)
  lazy val param: Parser[Param] =
    import Param.Kind.*
    opt("optional") ~ id ~ opt(":" ~> headParamType) ^^ {
      case opt ~ name ~ ty =>
        val kind = if (opt.isDefined) Optional else Normal
        Param(name, kind, ty.getOrElse("unknown"))
    } | opt(",") ~ "…" ^^^ Param("", Ellipsis, "")
  lazy val paramDesc: Parser[Param] =
    import Param.Kind.*
    headParamType ~ opt(id) ^^ {
      case ty ~ name => Param(name.getOrElse("this"), Normal, ty)
    }

  // syntax-directed operation (SDO) head generator
  lazy val sdoHeadGen: Parser[
    Option[SyntaxDirectedOperationHead.Target] => SyntaxDirectedOperationHead,
  ] =
    semanticsKind ~ name ~ params ^^ {
      case isStatic ~ x ~ params =>
        targetOpt => SyntaxDirectedOperationHead(targetOpt, x, isStatic, params)
    }

  // concrete method head generator
  lazy val concMethodHeadGen: Parser[Param => ConcreteMethodHead] =
    name ~ params ^^ {
      case name ~ params =>
        (receiverParam: Param) =>
          ConcreteMethodHead(name, receiverParam, params)
    }

  // internal method head generator
  lazy val inMethodHeadGen: Parser[Param => InternalMethodHead] =
    ("[[" ~> name <~ "]]") ~ params ^^ {
      case name ~ params =>
        (receiverParam: Param) =>
          InternalMethodHead(name, receiverParam, params)
    }

  // built-in heads
  lazy val builtinHead: Parser[BuiltinHead] =
    refName ~ params ^^ { case name ~ params => BuiltinHead(name, params) }
}
