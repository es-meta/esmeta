package esmeta.spec.util

import scala.io.Source
import esmeta.*
import esmeta.lang.Step
import esmeta.spec.{*, given}
import esmeta.util.BasicParsers
import esmeta.util.HtmlUtils.*
import esmeta.util.SystemUtils.*
import org.jsoup.nodes.*

/** specification parser */
object Parser extends Parsers {

  /** parses a specification */
  def parseSpec(
    content: String,
    version: Option[Spec.Version] = None,
  ): Spec =
    val document = content.toHtml
    val grammar = parseGrammar(document)
    val idxMap = grammar.idxMap
    val algorithms = parseAlgorithms(document, idxMap)
    val tables = parseTables(document)
    Spec(
      version = version,
      grammar = grammar,
      algorithms = algorithms,
      tables = tables,
      document = document,
    )

  /** parses a specification with versions */
  def parseSpecWithVersion(nameOpt: Option[String]): Spec =
    val cur = currentVersion(ECMA262_DIR)
    val (src, name, hash) = nameOpt match
      case Some(name) =>
        val hash = getVersion(name, ECMA262_DIR)
        changeVersion(hash, ECMA262_DIR)
        val src = readFile(SPEC_HTML)
        changeVersion(cur, ECMA262_DIR)
        (src, name, hash)
      case None =>
        (readFile(SPEC_HTML), cur, cur)
    parseSpec(src, Some(Spec.Version(cur, cur)))

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
    // TODO more general rules
    if (
      (parent.id endsWith "statement-rules") ||
      (parent.id endsWith "expression-rules")
    ) parent = parent.parent

    // checks whether it is an algorithm that should be ignored
    if (IGNORE_ALGO_PARENT_IDS contains parent.id) return Nil

    // checks whether it is a valid algorithm heaad
    if (parent.tagName != "emu-clause") return Nil

    // checks whether an element is of Chapter 5. Notational Conventions
    if (elem.isNotation) return Nil

    // consider algorithm head types using `type` attributes
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
        parseUnusualHead(parent, elem)
    }
  }

  /** parses tables */
  def parseTables(
    document: Document,
  ): Map[String, Table] = (for {
    elem <- document.getElems("emu-table")
    id = elem.getId
    datas = (for {
      row <- elem.getElems("tr")
    } yield row.getChildren.map(_.text)).toList
  } yield id -> Table(id, datas.head, datas.tail)).toMap

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

  // handle unusual heads
  lazy val thisValuePattern =
    "The abstract operation (this\\w+Value) takes argument _(\\w+)_.*".r
  private def parseUnusualHead(
    parent: Element,
    elem: Element,
  ): List[Head] = elem.getPrevText match
    case thisValuePattern(name, param) =>
      List(AbstractOperationHead(name, List(Param(param)), false))
    case _ => parseBuiltinHead(parent, elem)
}

/** specification parsers */
trait Parsers extends BasicParsers {
  // skip only white spaces and comments
  protected override val whiteSpace = "[ \t]*//.*|[ \t]+".r

  // production lists
  given prods: Parser[List[Production]] = {
    rep1(rep(newline) ~> prod)
  }.named("List[spec.Production]")

  // productions
  given prod: Parser[Production] = {
    lhs ~ prodKind ~ opt("one of") ~ rep1(opt(newline) ~> rhs) ^^ {
      case l ~ k ~ Some(_) ~ origRs =>
        val rs = for {
          r <- origRs
          s <- r.symbols
        } yield Rhs(None, List(s), None)
        Production(l, k, true, rs)
      case l ~ k ~ None ~ rs => Production(l, k, false, rs)
    }
  }.named("spec.Production")

  // production kinds
  lazy val prodKind: Parser[Production.Kind] =
    import Production.Kind.*
    ":::" ^^^ NumericString | "::" ^^^ Lexical | ":" ^^^ Syntactic

  // production left-hand-sides (LHSs)
  given lhs: Parser[Lhs] = {
    word ~ opt("[" ~> repsep(word, ",") <~ "]") ^^ {
      case name ~ params =>
        Lhs(name, params.getOrElse(Nil))
    }
  }.named("spec.Lhs")

  // production alternative right-hand-sides (RHSs)
  given rhs: Parser[Rhs] = {
    opt(rhsCond) ~ rep1(symbol) ~ opt(rhsId) ^^ {
      case c ~ ss ~ i =>
        Rhs(c, ss, i)
    }
  }.named("spec.Rhs")

  // RHS conditions
  given rhsCond: Parser[RhsCond] = {
    "[" ~> ("[+~]".r) ~ word <~ "]" ^^ {
      case str ~ name =>
        RhsCond(name, str == "+")
    }
  }.named("spec.RhsCond")

  // RHS ids
  lazy val rhsId: Parser[String] = "#" ~> "[-a-zA-Z0-9]+".r

  // grammar symbols
  given symbol: Parser[Symbol] = {
    term | butnot | lookahead | butOnlyIf | nt | abbr | unicodeSet | empty | nlt
  }.named("spec.Symbol")

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
  lazy val ntArg: Parser[NtArg] =
    import NtArg.Kind.*
    ("+" ^^^ True | "~" ^^^ False | "?" ^^^ Pass) ~ word ^^ {
      case kind ~ name => NtArg(kind, name)
    }

  // identifiers
  lazy val id: Parser[String] = "_[^_]+_".r ^^ { s =>
    s.substring(1, s.length - 1)
  }

  // names
  lazy val name: Parser[String] = "[a-zA-Z0-9/]+".r

  // TODO handle type more precisely such as:
  // - ~normal~, ~generator~, ~async~, or ~asyncGenerator~
  // - an ECMAScript language value, but not *undefined* or *null*
  // - a possibly empty List, each of whose elements is a String or *undefined*
  // ...
  given ty: Parser[Type] = "([^_,:]|, )+".r ^^ { Type(_) }

  lazy val ref: Parser[BuiltinHead.Ref] =
    import BuiltinHead.Ref
    import BuiltinHead.Ref.*
    lazy val name: Parser[String] = "[_`a-zA-Z0-9]+".r ^^ { _.trim }
    lazy val yet: Parser[String] = "[_`%a-zA-Z0-9.\\[\\]@ ]+".r ^^ { _.trim }
    lazy val pre: Parser[Ref => Ref] =
      "get " ^^^ { Getter(_) } | "set " ^^^ { Setter(_) } | "" ^^^ { x => x }
    lazy val base: Parser[Ref] =
      "%" ~> name <~ "%" ^^ { IntrinsicBase(_) } |
      name ^^ { NormalBase(_) }
    lazy val access: Parser[Ref => Ref] =
      "." ~> name ^^ { case n => NormalAccess(_, n) } |
      "[" ~> "@@" ~> name <~ "]" ^^ { case s => SymbolAccess(_, s) }
    pre ~ base ~ rep(access) <~ (guard("(") | not(".".r)) ^^ {
      case p ~ b ~ as => p(as.foldLeft(b) { case (b, a) => a(b) })
    } | yet ^^ { YetRef(_) }

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
    (ty <~ "::") ~ name ~ params ^^ {
      case t ~ x ~ ps => NumericMethodHead(t, x, ps)
    }

  // algorithm parameters
  lazy val param: Parser[Param] =
    import Param.Kind.*
    opt("optional") ~ id ~ opt(":" ~> ty) ^^ {
      case opt ~ name ~ ty =>
        val kind = if (opt.isDefined) Optional else Normal
        Param(name, kind, ty.getOrElse(UnknownType))
    } | opt(",") ~ "…" ^^^ Param("", Ellipsis, UnknownType)

  // multiple algorithm parameters
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
  lazy val paramDesc: Parser[Param] =
    import Param.Kind.*
    ty ~ opt(id) ^^ {
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
    ref ~ params ^^ { case name ~ params => BuiltinHead(name, params) }
}
