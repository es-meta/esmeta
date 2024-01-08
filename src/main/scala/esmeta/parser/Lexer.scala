package esmeta.parser

import esmeta.es.*
import esmeta.spec.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import scala.util.parsing.input.Position
import scala.util.matching.Regex

/** ECMAScript lexer */
trait Lexer extends UnicodeParsers {
  val grammar: Grammar

  // do not skip white spaces
  override def skipWhitespace = false

  // lexer type
  type Lexer = EPackratParser[String]

  extension (parser: Parser[String]) {
    // lookahead symbols
    def unary_- : Parser[String] = "" <~ not(parser)
    def unary_+ : Parser[String] = "" <~ guard(parser)

    // sequence
    def %(that: => Parser[String]): Parser[String] =
      parser ~ that ^^ { case x ~ y => x + y }

    // sequence with Skip
    def %%(that: => Parser[String]): Parser[String] = %(
      if (that eq strNoLineTerminator) that
      else Skip ~> that,
    )
  }

  // special code points
  lazy val WhiteSpaceCPs = USP ++ Set(TAB, VT, FF, SP, NBSP, ZWNBSP)
  lazy val LineTerminatorCPs = Set(LF, CR, LS, PS)
  lazy val NoLineTerminatorCPs = WhiteSpaceCPs -- LineTerminatorCPs

  // white space check
  def isWhiteSpace(cp: Int): Boolean = WhiteSpaceCPs contains cp

  // special lexers
  lazy val WhiteSpace = toParser(WhiteSpaceCPs)
  lazy val LineTerminator = toParser(LineTerminatorCPs)
  lazy val LineTerminatorSequence =
    val cr = toParser(CR)
    val lf = toParser(LF)
    toParser(LF, LS, PS) | cr <~ not(lf) | cr % lf
  lazy val Comment =
    """/\*+[^*]*\*+(?:[^/*][^*]*\*+)*/|//[^\u000A\u000D\u2028\u2029]*""".r
  lazy val empty = "".r
  lazy val Skip = rep(WhiteSpace | LineTerminator | Comment) ^^ { _.mkString }
  lazy val strNoLineTerminator =
    "" <~ guard(rep(WhiteSpace | Comment) ~ not(LineTerminator))

  // lexers
  lazy val lexers: Map[(String, Int), Lexer] = (for {
    prod <- grammar.prods
    if prod.kind == ProductionKind.Lexical
    name = prod.lhs.name
    size = prod.lhs.params.length
    argsBit <- 0 until 1 << size
    args = toBools(size, argsBit)
    argsSet = getArgs(prod.lhs.params, args)
    lexer = getLexer(prod, argsSet)
  } yield (name, argsBit) -> lexer).toMap

  lazy val lexNames = lexers.keySet.map(_._1)

  // internal data for packrat parsers
  type ParseCase[+T]
  protected case class Data(
    var cache: Map[ParseCase[Any], ParseResult[_]] = Map(),
    var rightmostFailedPos: Option[(Position, List[Elem])] = None,
    var rightmostDoWhileClose: Option[Position] = None,
  ) extends DataType { def next = this }
  protected def defaultData = Data()

  // get a lexer from a lexical production with an argument map
  protected def getLexer(
    prod: Production,
    argsSet: Set[String],
  ): Lexer = {
    lazy val parser = prod.rhsList.map(getRhsParser(_, argsSet)).reduce(_ ||| _)
    parser
  }

  protected val FAIL: Parser[Nothing] = failure("Lexer")

  // get a parser for a right-hand side (RHS)
  protected def getRhsParser(rhs: Rhs, argsSet: Set[String]): Parser[String] =
    if (rhs.available(argsSet))
      rhs.symbols
        .map(getSymbolParser(_, argsSet))
        .reduce(_ % _)
    else FAIL

  // get a parser for a symbol
  protected def getSymbolParser(
    symbol: Symbol,
    argsSet: Set[String],
  ): Parser[String] = symbol match
    case Terminal(term) => term
    case Nonterminal(name, args) =>
      lexers((name, toBit(toBools(argsSet, args))))
    case Optional(symbol) =>
      opt(getSymbolParser(symbol, argsSet)) ^^ { _.getOrElse("") }
    case ButNot(base, cases) =>
      val parser = getSymbolParser(base, argsSet)
      val exclude = cases.map(getSymbolParser(_, argsSet)).reduce(_ ||| _)
      parser.filter(parseAll(exclude, _).isEmpty)
    case ButOnlyIf(base, name, cond) =>
      getSymbolParser(base, argsSet) // TODO more precise
    case Lookahead(b, cases) =>
      val parser = cases
        .map(_.map(getSymbolParser(_, argsSet)).reduce(_ % _))
        .reduce(_ ||| _)
      "" <~ (if (b) guard else not)(parser)
    case Empty               => EMPTY
    case NoLineTerminator    => strNoLineTerminator
    case CodePoint(cp, desc) => cp.toChar.toString.r
    case CodePointAbbr(abbr) =>
      abbrCPs
        .getOrElse(
          abbr,
          error(s"unknown code point abbreviation: <$abbr>"),
        )
        .map(_.toChar)
        .mkString("[", "", "]")
        .r
    case UnicodeSet(None)                                         => Any
    case UnicodeSet(Some("with the Unicode property “ID_Start”")) => IDStart
    case UnicodeSet(Some("with the Unicode property “ID_Continue”")) =>
      IDContinue
    case UnicodeSet(Some(cond)) => ???

  // get a set of argument names from lists of parameters and arguments
  protected def getArgs(
    params: List[String],
    args: List[Boolean],
  ): Set[String] =
    (params zip args).collect { case (p, true) => p }.toSet

  // conversion boolean arguments to a bit representation
  protected def toBit(args: List[Boolean]): Int =
    args.foldLeft(0) { case (bit, arg) => (bit << 1) + (if (arg) 1 else 0) }

  // conversion a bit representation to boolean arguments
  protected def toBools(size: Int, argsBit: Int): List[Boolean] =
    (for (k <- 0 until size) yield ((1 << k) & argsBit) > 0).toList

  // conversion a list of nonterminal arguments to boolean arguments
  protected def toBools(
    argsSet: Set[String],
    args: List[NonterminalArgument],
  ): List[Boolean] =
    import NonterminalArgumentKind.*
    args.map {
      case NonterminalArgument(True, _)    => true
      case NonterminalArgument(False, _)   => false
      case NonterminalArgument(Pass, name) => argsSet contains name
    }
}
