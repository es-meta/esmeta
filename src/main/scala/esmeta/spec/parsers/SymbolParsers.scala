package esmeta.spec.parsers

import esmeta.spec.*
import scala.util.parsing.combinator.*

/** parsers for grammar symbols */
trait SymbolParsers extends BaseParsers {
  import Symbol.*
  import NtArg.Kind.*

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
