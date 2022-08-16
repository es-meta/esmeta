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

  // special lexers
  lazy val WhiteSpace = TAB | VT | FF | SP | NBSP | ZWNBSP | USP
  lazy val LineTerminator = LF | CR | LS | PS
  lazy val LineTerminatorSequence =
    LF | CR <~ not(LF) | LS | PS | CR % LF
  lazy val Comment =
    """/\*+[^*]*\*+(?:[^/*][^*]*\*+)*/|//[^\u000A\u000D\u2028\u2029]*""".r
  lazy val empty = "".r
  lazy val Skip =
    rep(WhiteSpace | LineTerminator | Comment) ^^ { _.mkString }
  lazy val strNoLineTerminator =
    "" <~ guard(Skip.filter(s => lines.findFirstIn(s).isEmpty))

  // lexers
  lazy val lexers: Map[(String, Int), Lexer] = (for {
    prod <- grammar.prods
    if prod.kind == Production.Kind.Lexical
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
    rhs.condition match {
      case Some(RhsCond(name, pass)) if (argsSet contains name) != pass => FAIL
      case _ =>
        rhs.symbols
          .map(getSymbolParser(_, argsSet))
          .reduce(_ % _)
    }

  // get a parser for a symbol
  protected def getSymbolParser(
    symbol: Symbol,
    argsSet: Set[String],
  ): Parser[String] = symbol match
    case Terminal(term) => term
    case Nonterminal(name, args, optional) =>
      val parser = lexers((name, toBit(toBools(argsSet, args))))
      if (optional) opt(parser) ^^ { _.getOrElse("") }
      else parser
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
    case Empty            => EMPTY
    case NoLineTerminator => strNoLineTerminator
    case CodePointAbbr(abbr) =>
      abbrCPs.getOrElse(
        abbr,
        error(s"unknown code point abbreviation: <$abbr>"),
      )
    case UnicodeSet(None)                                         => Unicode
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
    args: List[NtArg],
  ): List[Boolean] =
    import NtArg.Kind.*
    args.map {
      case NtArg(True, _)    => true
      case NtArg(False, _)   => false
      case NtArg(Pass, name) => argsSet contains name
    }
}
