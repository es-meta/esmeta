package esmeta.js.util

import esmeta.js.*
import esmeta.spec.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import scala.util.parsing.input.Position
import scala.util.matching.Regex

/** JavaScript parser */
class Parser(val grammar: Grammar) extends BasicParsers with EPackratParsers {
  // do not skip white spaces
  override def skipWhitespace = false

  // lexer type
  type Lexer = EPackratParser[String]

  val abbrCPs: Map[String, Regex] = Map(
    // unicode format-control characters
    "ZWNJ" -> "\u200C".r,
    "ZWJ" -> "\u200D".r,
    "ZWNBSP" -> "\uFEFF".r,
    // white spaces
    "TAB" -> "\u0009".r,
    "VT" -> "\u000B".r,
    "FF" -> "\u000C".r,
    "SP" -> "\u0020".r,
    "NBSP" -> "\u00A0".r,
    // TODO automatically extract category "Zs"
    "USP" ->
    "[\u1680\u2000\u2001\u2002\u2003\u2004\u2005\u2006\u2007\u2008\u2009\u200A\u202F\u205F\u3000]".r,
    // line terminators
    "LF" -> "\u000A".r,
    "CR" -> "\u000D".r,
    "LS" -> "\u2028".r,
    "PS" -> "\u2029".r,
  )

  lazy val lines = "[\u000A\u000D\u2028\u2029]".r

  lazy val Unicode = "(?s).".r
  lazy val Comment =
    """/\*+[^*]*\*+(?:[^/*][^*]*\*+)*/|//[^\u000A\u000D\u2028\u2029]*""".r
  lazy val IDStart =
    Unicode.filter(s => CodePoint.IDStart contains toCodePoint(s))
  lazy val IDContinue =
    Unicode.filter(s => CodePoint.IDContinue contains toCodePoint(s))

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
      abbrCPs.getOrElse(
        abbr,
        error(s"unknown code point abbreviation: <$abbr>"),
      )
    case Nonterminal(name, args, optional) =>
      val parser = lexers((name, toBit(toBools(argsSet, args))))
      if (optional) opt(parser) ^^ { _.getOrElse("") }
      else parser
    case Lookahead(b, cases)         => ???
    case ButOnlyIf(base, name, cond) => ???
    case UnicodeSet(None)            => Unicode
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

  private def toCodePoint(str: String): Int =
    def check4B(i: Int): Boolean =
      str.codePointCount(i, str.length min (i + 2)) == 1
    def aux(i: Int, acc: Int): Int =
      if (i >= str.length) acc
      else
        val nextAcc = str.codePointAt(i) + (acc * (1 << 16))
        aux(if (check4B(i)) i + 2 else i + 1, nextAcc)
    aux(0, 0)
}
