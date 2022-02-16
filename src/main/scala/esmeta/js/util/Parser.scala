package esmeta.js.util

import esmeta.*
import esmeta.error.*
import esmeta.js.*
import esmeta.spec.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import scala.util.parsing.combinator.*
import scala.util.parsing.input.*
import scala.util.matching.Regex

/** JavaScript parser */
case class Parser(val grammar: Grammar) extends LAParsers {
  def lex(name: String, k: Int = 0): String => String =
    str => parseAll(lexers((name, k)), str).get

  // terminal lexer
  protected val TERMINAL: Lexer = grammar.prods.foldLeft[Parser[String]]("") {
    case (parser, Production(lhs, _, _, rhsList)) =>
      rhsList.foldLeft(parser) {
        case (parser, Rhs(_, tokens, _)) =>
          tokens.foldLeft(parser) {
            case (parser, Terminal("?.")) => parser ||| ("?." <~ not("\\d".r))
            case (parser, Terminal(t))    => parser ||| t
            case (parser, _)              => parser
          }
      }
  }

  // automatic semicolon insertion
  def insertSemicolon(reader: EPackratReader[Char]): Option[String] = {
    reader.data.rightmostFailedPos match {
      case Some((pos, rev)) =>
        val source = reader.source.toString
        val line = pos.line - 1
        val column = pos.column - 1
        val lines = source.replace("\r\n", "\n").split(Array('\n', '\r'))
        val revStr = rev.mkString

        // Interactive debugging for semicolon insertion
        if (DEBUG && keepLog) {
          lines.zipWithIndex.foreach {
            case (x, i) => println(f"$i%4d: $x")
          }
          stop(s"line: $line, column: $column") match {
            case "q" => keepLog = false
            case _   =>
          }
        }

        lazy val curLine = lines(line)
        lazy val curChar = curLine.charAt(column)

        // insert semicolon right before the offending token
        lazy val insert = Some({
          if (line < lines.length && column < curLine.length) {
            val (pre, post) = curLine.splitAt(column)
            lines(line) = pre + ';' + post
            lines.mkString("\n")
          } else lines.mkString("\n") + "\n;"
        })

        // 2. The end of the input stream of tokens is encountered
        if (
          line >= lines.length ||
          (line == lines.length - 1 && column == curLine.length)
        ) return insert

        // A. Additional Rules
        // A semicolon is never inserted automatically if the semicolon
        // would then be parsed as an empty statement or if that semicolon
        // would become one of the two semicolons in the header of a for statement
        // TODO
        if (curChar == ';') return None

        // 1-1. The offending token is separated from the previous token
        //      by at least one LineTerminator
        if (!parse(strNoLineTerminator, revStr).successful) return insert

        // 1-2. The offending token is '}'
        if (curChar == '}') return insert

        // 1-3. The previous token is ')' and the inserted semicolon would then be
        //      parsed as the terminating semicolon of a do-while statement (13.7.2).
        reader.data.rightmostDoWhileClose match {
          case Some(doWhilePos) if doWhilePos == pos => return insert
          case _                                     =>
        }

        // 3. the restricted token is separated from the previous token
        //    by at least one LineTerminator, then a semicolon is automatically
        //    inserted before the restricted token.
        // TODO

        None
      case None => None
    }
  }

  // terminal parsers
  val t = cached[String, LAParser[String]] {
    case t =>
      log(
        new LAParser(
          follow => {
            Skip ~> {
              if (t.matches("[a-z]+")) t <~ not(IDContinue)
              else t
            } <~ +follow.parser
          },
          FirstTerms() + t,
        ),
      )(t)
  }
  val doWhileCloseT: LAParser[String] = {
    val p = t(")")
    new LAParser(
      follow =>
        Parser { rawIn =>
          val in = rawIn.asInstanceOf[EPackratReader[Char]]
          val c = ParseCase(p, follow, in.pos)
          val data = in.data
          data.cache.get(c) match {
            case Some(res) => res.asInstanceOf[ParseResult[String]]
            case None =>
              val res = recordDoWhileClose(p.parser(follow), in)
              data.cache += c -> res
              res
          }
        },
      p.first,
    )
  }
  val nt = cached[(String, Lexer), LAParser[Lexical]] {
    case (name, nt) =>
      log(
        new LAParser(
          follow =>
            (Skip ~> nt <~ +follow.parser) ^^ { case s => Lexical(name, s) },
          FirstTerms() + (name -> nt),
        ),
      )(name)
  }
  def ntl = cached[Lexer, LAParser[Lexical]] {
    case nt =>
      log(
        new LAParser(
          follow => (Skip ~> nt) ^^ { case s => Lexical("", s) },
          FirstTerms(),
        ),
      )("")
  }

  // parser that supports automatic semicolon insertions
  override def parse[T](p: LAParser[T], in: Reader[Char]): ParseResult[T] = {
    val MAX_ADDITION = 100
    val init: Either[ParseResult[T], Reader[Char]] = Right(in)
    (0 until MAX_ADDITION).foldLeft(init) {
      case (Right(in), _) =>
        val reader = new EPackratReader(in)
        p(emptyFirst, reader) match {
          case (f: Failure) =>
            insertSemicolon(reader) match {
              case Some(str) => Right(new CharSequenceReader(str))
              case None      => Left(f)
            }
          case r => Left(r)
        }
      case (res, _) => res
    } match {
      case Left(res) => res
      case _         => throw TooManySemicolonInsertion(MAX_ADDITION)
    }
  }

  // ECMAScript parsers
  type ESParser[+T] = List[Boolean] => LAParser[T]

  // memoization of parametric rules
  def memo[T](f: ESParser[T]): ESParser[T] = cached(args => memo(f(args)))

  // resolve left recursions
  type FLAParser[T] = LAParser[T => T]
  def resolveLR[T](f: LAParser[T], s: FLAParser[T]): LAParser[T] = {
    lazy val p: FLAParser[T] = s ~ p ^^ {
      case b ~ f => (x: T) => f(b(x))
    } | MATCH ^^^ { (x: T) => x }
    f ~ p ^^ { case a ~ f => f(a) }
  }

  // record right-most faield positions
  protected def record[T](
    parser: Parser[T],
    in: EPackratReader[Char],
  ): ParseResult[T] = {
    val data = in.data
    val res = parser(in)
    (res, data.rightmostFailedPos) match {
      case (f @ Failure(_, cur: EPackratReader[_]), Some((origPos, _)))
          if origPos < cur.pos =>
        data.rightmostFailedPos = Some((cur.pos, cur.rev))
      case (f @ Failure(_, cur: EPackratReader[_]), None) =>
        data.rightmostFailedPos = Some((cur.pos, cur.rev))
      case _ =>
    }
    res
  }

  // record right-most do-while close token positions
  protected def recordDoWhileClose[T](
    parser: Parser[T],
    in: EPackratReader[Char],
  ): ParseResult[T] = {
    val data = in.data
    val res = parser(in)
    (res, data.rightmostDoWhileClose) match {
      case (f @ Failure(_, cur: EPackratReader[_]), Some(origPos))
          if origPos < cur.pos =>
        data.rightmostDoWhileClose = Some(cur.pos)
      case (f @ Failure(_, cur: EPackratReader[_]), None) =>
        data.rightmostDoWhileClose = Some(cur.pos)
      case _ =>
    }
    res
  }

  // script parsers
  lazy val Script: ESParser[Ast] = ???

  // no LineTerminator parser
  lazy val NoLineTerminator: LAParser[String] = log(
    new LAParser(
      follow => strNoLineTerminator,
      emptyFirst,
    ),
  )("NoLineTerminator")

  // parsers
  lazy val parsers: Map[String, ESParser[Ast]] = ???

  // get fixed length arguments
  def getArgsN(name: String, args: List[Boolean], n: Int): List[Boolean] = {
    if (args.length == n) args
    else throw WrongNumberOfParserParams(name, args)
  }
}
