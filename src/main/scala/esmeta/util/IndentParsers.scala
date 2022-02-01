package esmeta.util

import scala.util.parsing.combinator.*
import scala.util.parsing.input.{Reader, Position}
import scala.collection.mutable

/** parsers for indentations defined with packrat parsing */
trait IndentParsers extends BasicParsers with EPackratParsers {
  // extended data for indentation
  case class Data(
    indents: List[Int] = Nil,
    steps: List[Int] = Nil,
    needUppercase: Boolean = false,
  ) extends DataType { def next = Data(indents, steps, false) }
  val defaultData = Data()

  /** end of line */
  val EOL = "\n|$".r

  /** treat only spaces as white spaces */
  override val whiteSpace = "( |\n *$)+".r

  /** a next parser */
  val next: Parser[Unit] = new Parser[Unit] {
    val name = "next"
    def apply(in: Input) = in match {
      case in: In =>
        newlineWithSpaces(in) match {
          case Success(expected, newIn: In) =>
            val data @ Data(indents, steps, needUppercase) = in.data
            indents match {
              case indent :: _ if expected != indent =>
                Failure(s"expected $expected but $indent indentations", in)
              case _ =>
                val newSteps = steps match
                  case Nil       => Nil
                  case s :: rest => (s + 1) :: rest
                Success((), newIn.copy(Data(indents, newSteps, false)))
            }
          case Success(_, newIn) => Failure("not an EPackratReader", newIn)
          case fail: NoSuccess   => fail
        }
      case in => Failure("not an EPackratReader", in)
    }
  }

  /** a indent parser */
  val indent: Parser[Unit] = new Parser[Unit] {
    val name = "indent"
    def apply(in: Input) = in match {
      case in: In =>
        val data @ Data(indents, steps, needUppercase) = in.data
        (indents, newlineWithSpaces(in)) match {
          case (Nil, Success(indent, _)) => Success((), push(in, indent))
          case (prevIndent :: _, Success(indent, _)) =>
            if (prevIndent < indent) Success((), push(in, indent))
            else Failure(s"> $prevIndent expected but $indent indentations", in)
          case (_, fail: NoSuccess) => fail
        }
      case in => Failure("not an EPackratReader", in)
    }
  }

  /** a dedent parser */
  val dedent: Parser[Unit] = new Parser[Unit] {
    val name = "dedent"
    def apply(in: Input) = in match {
      case in: In =>
        val data @ Data(indents, steps, needUppercase) = in.data
        (pop(in), newlineWithSpaces(in)) match {
          case (None, _) => Failure("no indentation for dedent", in)
          case (Some((prevIndent, newIn)), Success(indent, _)) =>
            if (indent < prevIndent) Success((), newIn)
            else Failure(s"< $prevIndent expected but $indent indentations", in)
          case (_, fail: NoSuccess) => fail
        }
      case in => Failure("not an EPackratReader", in)
    }
  }

  /** turn on needUppercase symbols */
  val upper: Parser[Unit] = new Parser[Unit] {
    def apply(in: Input) = in match {
      case in: In =>
        Success((), in.copy(in.data.copy(needUppercase = true)))
      case in => Failure("not an EPackratReader", in)
    }
  }

  /** a parser that matches a literal string */
  override implicit def literal(s: String): Parser[String] =
    val superLiteral = super.literal
    new Parser[String] {
      def apply(in: Input) = superLiteral(in match {
        case in: In if in.data.needUppercase && s.length > 0 =>
          s"${s.head.toUpper}${s.substring(1)}"
        case _ => s
      })(in)
    }

  // ------------------------------------------------------------------------------
  // locational parser
  // ------------------------------------------------------------------------------
  abstract class LocationalParser[+T <: Locational] extends Parser[T]
  def locationed[T <: Locational](p: => Parser[T]): LocationalParser[T] =
    new LocationalParser {
      def apply(in: Input) = in match
        case in: In =>
          val trimmed = trimInput(in)
          p(trimmed) match
            case s @ Success(res, rest) =>
              new Success(
                res.setLoc(trimmed.pos, rest.pos, trimmed.data.steps.reverse),
                rest,
              )
            case ns: NoSuccess => ns
        case in => Failure("not an IndentReader", in)
    }

  // trim unused whitespace for position
  private def trimInput[T](in: In): In =
    val offset = in.offset
    val start = handleWhiteSpace(in.source, offset)
    val trimmed = in.drop(start - offset).asInstanceOf[In]
    trimmed.copy(trimmed.data.copy(needUppercase = in.data.needUppercase))

  /** implicit conversion from scala parser's Position to util.Pos */
  private given Conversion[Position, Pos] with
    def apply(p: Position): Pos = Pos(p.line, p.column)

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  // a parser for newline with spaces
  private val newlineWithSpaces: EPackratParser[Int] =
    "\n *".r ^^ { _.length - 1 } | "$".r ^^^ 0

  // push a new indentation
  private def push(in: In, indent: Int): In =
    val data @ Data(indents, steps, _) = in.data
    in.copy(Data(indent :: indents, 0 :: steps, false))

  // pop an indentation
  private def pop(in: In): Option[(Int, In)] =
    val data @ Data(indents, steps, needUppercase) = in.data
    indents match
      case Nil => None
      case indent :: remain =>
        Some(indent, in.copy(Data(remain, steps.tail, false)))

  // packrat reader for characters
  private type In = EPackratReader[Char]
}
