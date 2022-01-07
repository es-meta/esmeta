package esmeta.spec

import esmeta.LINE_SEP
import esmeta.util.FileUtils.*
import esmeta.util.Useful.*
import org.jsoup.nodes.Document
import scala.util.parsing.combinator.*

/** specification parsing rules */
trait Parsers extends RegexParsers {
  import Production.Kind.*
  import NtArg.Kind.*
  import Symbol.*

  // common parsers
  protected override val whiteSpace = "[ \t]*//.*|[ \t]+".r
  lazy val newline = "\r?\n|\r|\f".r
  lazy val word = "\\w+".r

  // productions
  lazy val prods: Parser[List[Production]] =
    rep1(rep(newline) ~> prod)

  // productions
  lazy val prod: Parser[Production] =
    lhs ~ prodKind ~ opt("one of") ~ rep1(newline ~> rhs) ^^ {
      case l ~ k ~ Some(_) ~ origRs =>
        val rs =
          for (r <- origRs; s <- r.symbols)
            yield Rhs(None, List(s), None)
        Production(l, k, true, rs)
      case l ~ k ~ None ~ rs =>
        Production(l, k, false, rs)
    }

  // production kinds
  lazy val prodKind: Parser[Production.Kind] =
    ":::" ^^^ NumericString | "::" ^^^ Lexical | ":" ^^^ Normal

  // production left-hand-sides (LHSs)
  lazy val lhs: Parser[Lhs] =
    word ~ opt("[" ~> repsep(word, ",") <~ "]") ^^ { case name ~ params =>
      Lhs(name, params.getOrElse(Nil))
    }

  // production alternative right-hand-sides (RHSs)
  lazy val rhs: Parser[Rhs] =
    opt(rhsCond) ~ rep1(symbol) ~ opt(rhsId) ^^ { case c ~ ss ~ i =>
      Rhs(c, ss, i)
    }
  lazy val rhsCond: Parser[RhsCond] =
    "[" ~> ("[+~]".r) ~ word <~ "]" ^^ { case str ~ name =>
      RhsCond(name, str == "+")
    }
  lazy val rhsId: Parser[String] = "#" ~> "[-a-zA-Z0-9]+".r

  // grammar symbols
  lazy val symbol: Parser[Symbol] =
    term | butnot | lookahead | nt | empty | nlt | character | butnot | lookahead | nt | term

  // terminals
  lazy val term: Parser[Terminal] =
    "`[^`]+`|```".r ^^ { case str =>
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

  // empty symbols
  lazy val empty: Parser[Empty.type] = "[empty]" ^^^ Empty

  // no-line-terminator symbols
  lazy val nlt: Parser[NoLineTerminator.type] =
    "\\[no [\\|]?LineTerminator[\\|]? here\\]".r ^^^ NoLineTerminator

  // character symbols
  lazy val character: Parser[Symbol] = (
    "<" ~> word <~ ">" ^^ { Unicode(_) } |
      ".*code point.*ID_Start.*".r ^^^ UnicodeIdStart |
      ".*code point.*ID_Continue.*".r ^^^ UnicodeIdContinue |
      ".*code point.*0xD800 to 0xDBFF.*".r ^^^ UnicodeLeadSurrogate |
      ".*code point.*0xDC00 to 0xDFFF.*".r ^^^ UnicodeTrailSurrogate |
      ".*any.*code point.*".r ^^^ UnicodeAny |
      ".*HexDigits.*> 0x10FFFF.*".r ^^^ NotCodePoint |
      ".*HexDigits.*≤ 0x10FFFF.*".r ^^^ CodePoint |
      ".*Hex4Digits.*0xD800 to 0xDBFF.*".r ^^^ HexLeadSurrogate |
      ".*Hex4Digits.*0xDC00 to 0xDFFF.*".r ^^^ HexTrailSurrogate |
      ".*Hex4Digits.*not.*0xD800 to 0xDFFF.*".r ^^^ HexNonSurrogate |
      ".*DecimalEscape.*CapturingGroupNumber.*|DecimalEscape|.*≤.*_NcapturingParens.*".r ^^^ NonUnicodeModeDecimalEscape
  )

  // lookahead symbol
  lazy val lookahead = "[lookahead " ~> containsSymbol ~ laList <~ "]" ^^ {
    case b ~ cases => Lookahead(b, cases)
  }
  lazy val laList = opt("{") ~> repsep(rep(symbol), ",") <~ opt("}")
  lazy val containsSymbol =
    ("==" | "<" | "∈") ^^^ true | ("!=" | "<!" | "∉") ^^^ false

  // nonterminal arguments
  lazy val ntArg: Parser[NtArg] =
    ("+" ^^^ True | "~" ^^^ False | "?" ^^^ Pass) ~ word ^^ {
      case kind ~ name => NtArg(kind, name)
    }

}

/** specification parser */
object Parser extends Parsers {

  /** parses a specification */
  def parseSpec(content: String): Spec = {
    val document = readHtml(content)
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
