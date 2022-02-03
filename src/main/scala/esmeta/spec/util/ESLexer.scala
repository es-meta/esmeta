package esmeta.spec.util

import esmeta.spec.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import scala.util.parsing.input.Position

/** A lexer for ECMAScript programs */
class ESLexer(val grammar: Grammar) extends BasicParsers with EPackratParsers {
  // do not skip white spaces
  override def skipWhitespace = false

  // lexer type
  type Lexer = EPackratParser[String]

  // lexers
  val lexers: Map[(String, Int), Lexer] = (for {
    prod <- grammar.prods
    if prod.kind == Production.Kind.Lexical
    name = prod.lhs.name
    size = prod.lhs.params.length
    argsBit <- 0 until 1 << size
    args = toBools(size, argsBit)
    argsSet = getArgs(prod.lhs.params, args)
    lexer = getLexer(prod, argsSet)
  } yield (name, argsBit) -> lexer).toMap

  // internal data for packrat parsers
  protected case class Data(
    var rightmostFailedPos: Option[(Position, List[Elem])] = None,
    var rightmostDoWhileClose: Option[Position] = None,
  ) extends DataType { def next = this }
  protected val defaultData = Data()

  // get a lexer from a lexical production with an argument map
  private def getLexer(
    prod: Production,
    argsSet: Set[String],
  ): Lexer = {
    lazy val parser = prod.rhsList.map(getRhsParser(_, argsSet)).reduce(_ ||| _)
    parser
  }

  private val FAIL: Parser[Nothing] = failure("Lexer")

  // get a parser for a right-hand side (RHS)
  private def getRhsParser(rhs: Rhs, argsSet: Set[String]): Parser[String] =
    rhs.condition match {
      case Some(RhsCond(name, pass)) if (argsSet contains name) != pass => FAIL
      case _ =>
        rhs.symbols
          .map(getSymbolParser(_, argsSet))
          .reduce(_ ~ _ ^^ { case x ~ y => x + y })
    }

  // get a parser for a symbol
  private def getSymbolParser(
    symbol: Symbol,
    argsSet: Set[String],
  ): Parser[String] = symbol match {
    case Terminal(term) => term
    case ButNot(base, cases) =>
      val parser = getSymbolParser(base, argsSet)
      val exclude = cases.map(getSymbolParser(_, argsSet)).reduce(_ ||| _)
      parser.filter(parseAll(exclude, _).isEmpty)
    case Empty => ""
    case CodePointAbbr(abbr) =>
      abbr match {
        case "ZWNJ"   => "\u200C"
        case "ZWJ"    => "\u200D"
        case "ZWNBSP" => "\uFEFF"
        case "TAB"    => "\u0009"
        case "VT"     => "\u000B"
        case "FF"     => "\u000C"
        case "SP"     => "\u0020"
        case "NBSP"   => "\u00A0"
        case "USP" =>
          "[\u1680\u2000\u2001\u2002\u2003\u2004\u2005\u2006\u2007\u2008\u2009\u200A\u202F\u205F\u3000]".r
        case "LF" => "\u000A"
        case "CR" => "\u000D"
        case "LS" => "\u2028"
        case "PS" => "\u2029"
        case _    => error(s"unknown code point abbreviation: <$abbr>")
      }
    case Nonterminal(name, args, optional) =>
      val parser = lexers((name, toBit(toBools(argsSet, args))))
      if (optional) opt(parser) ^^ { _.getOrElse("") }
      else parser
    case Lookahead(b, cases)         => ???
    case ButOnlyIf(base, name, cond) => ???
    case UnicodeSet(None)            => "(?s).".r
    case UnicodeSet(cond)            => ???
    case _ => error(s"invalid symbol in lexer: $symbol")
  }

  // get a set of argument names from lists of parameters and arguments
  private def getArgs(params: List[String], args: List[Boolean]): Set[String] =
    (params zip args).collect { case (p, true) => p }.toSet

  // conversion boolean arguments to a bit representation
  private def toBit(args: List[Boolean]): Int =
    args.foldLeft(0) { case (bit, arg) => (bit << 1) + (if (arg) 1 else 0) }

  // conversion a bit representation to boolean arguments
  private def toBools(size: Int, argsBit: Int): List[Boolean] =
    (for (k <- 0 until size) yield ((1 << k) & argsBit) > 0).toList

  // conversion a list of nonterminal arguments to boolean arguments
  private def toBools(argsSet: Set[String], args: List[NtArg]): List[Boolean] =
    import NtArg.Kind.*
    args.map {
      case NtArg(True, _)    => true
      case NtArg(False, _)   => true
      case NtArg(Pass, name) => argsSet contains name
    }
}
