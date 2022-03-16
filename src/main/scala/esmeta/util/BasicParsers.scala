package esmeta.util

import esmeta.util.BaseUtils.*
import esmeta.util.{Loc, Pos}
import java.io.*
import java.nio.charset.Charset
import scala.util.parsing.combinator.JavaTokenParsers

/** basic parsers */
trait BasicParsers extends JavaTokenParsers {
  // parse from file
  def fromFileWithParser[T](f: String, parser: Parser[T]): T = {
    var fileName = new File(f).getCanonicalPath
    val fs = new FileInputStream(new File(f))
    val sr = new InputStreamReader(fs, Charset.forName("UTF-8"))
    val in = new BufferedReader(sr)
    val result = errHandle(parser, parseAll(parser, in))
    in.close; sr.close; fs.close
    result
  }

  /** white spaces with comments */
  lazy val whiteSpaceWithComment =
    """(\s|/\*+[^*]*\*+(?:[^/*][^*]*\*+)*/|//[^\u000A\u000D\u2028\u2029]*)+""".r

  // parse with error message
  def errHandle[T](parser: Parser[T], result: ParseResult[T]): T = result match
    case Success(result, _) => result
    case err                => error(s"[$parser] $err")

  // parse
  def parseBy[T](parser: Parser[T])(str: String): T =
    errHandle(parser, parseAll(parser, str))
  def parse[T](str: String)(using parser: Parser[T]): T =
    parseBy(parser)(str)

  // string literal
  lazy val string = ("\"[\u0000-\u000F]\"".r | stringLiteral) ^^ {
    case s =>
      StringContext processEscapes s.substring(1, s.length - 1)
  }

  // line terminator
  lazy val newline = "\r?\n|\r|\f".r

  // any word
  lazy val word = "\\w+".r

  // camel case
  lazy val camel = "[A-Z][a-zA-Z0-9]+".r

  // boolean
  lazy val bool = "true" ^^^ true | "false" ^^^ false

  // integers
  lazy val integer = "(0|-?[1-9]\\d*)".r
  lazy val int = integer ^^ { _.toInt }

  // numbers
  lazy val number = "[+-]?(0|[1-9][0-9]*)(\\.[0-9]+)?".r
  lazy val double = number ^^ { _.toDouble }
  lazy val bigint = integer ^^ { BigInt(_) }
  lazy val codeUnit = integer ^^ { _.toInt.toChar }
  lazy val decimal = number ^^ { BigDecimal.exact(_) }

  trait From[T](using parser: Parser[T]) {
    def fromFile(str: String): T =
      fromFileWithParser(str, parser)
    def from(str: String): T =
      parseBy[T](parser)(str)
  }

  // locations
  lazy val pos: Parser[Pos] = int ~ (":" ~> int) ^^ { case l ~ c => Pos(l, c) }
  lazy val loc: Parser[Loc] = {
    lazy val steps = "(" ~> repsep(int, ".") <~ ")"
    pos ~ ("-" ~> pos) ~ steps ^^ { case s ~ e ~ ss => Loc(s, e, ss) }
  }
}
