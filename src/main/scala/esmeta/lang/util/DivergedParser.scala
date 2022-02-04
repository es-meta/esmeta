package esmeta.lang.util

import esmeta.lang.*
import esmeta.util.BaseUtils.error
import esmeta.util.IndentParsers

/** parsers for record diverged case with indentation parsers */
trait DivergedParsers extends IndentParsers {

  /** extended data for recording divergence */
  class DataWithDiverged(
    val d: Data = Data(),
    val diverged: List[Map[String, Int]] = List(),
  ) extends Data(d.indents, d.steps, d.needUppercase):
    /** override methods of data */
    override def copy(
      indents: List[Int] = d.indents,
      steps: List[Int] = d.steps,
      needUppercase: Boolean = d.needUppercase,
    ): Data =
      DataWithDiverged(super.copy(indents, steps, needUppercase), diverged)
    override def next = DataWithDiverged(d.next, diverged)

    /** push, pop new diverge context */
    def push = DataWithDiverged(d, Map() :: diverged)
    def pop = diverged match {
      case m :: rest => (m, DataWithDiverged(d, rest))
      case _         => error("stack is empty")
    }

    /** record kind to current diverge context */
    def record(key: String, kind: Int) = diverged match
      case m :: rest => DataWithDiverged(d, (m + (key -> kind)) :: rest)
      case _         => this

  /** default data */
  override val defaultData = DataWithDiverged()

  /** start record */
  private val start: Parser[Unit] = new Parser[Unit] {
    val name = "diverge-start"
    def apply(in: Input) = handleReader(in) { in =>
      val newData = in.data match
        case diverged: DataWithDiverged => diverged.push
        case data                       => data
      Success((), in.copy(newData))
    }
  }

  /** end record */
  private val end: Parser[Map[String, Int]] = new Parser[Map[String, Int]] {
    val name = "diverge-end"
    def apply(in: Input) = handleReader(in) { in =>
      val (m, newData) = in.data match
        case diverged: DataWithDiverged => diverged.pop
        case data                       => (Map[String, Int](), data)
      Success(m, in.copy(newData))
    }
  }

  /** basic helpers for record kind */
  private def recordKind[T](
    result: ParseResult[T],
    key: String,
    kind: Int,
  ): ParseResult[T] =
    result match
      case s @ Success(res, in: In) =>
        in.data match
          case diverged: DataWithDiverged =>
            Success(res, in.copy(diverged.record(key, kind)))
          case _ => s
      case _ => result

  /** record append(`|`) */
  def recordAppend[T](
    name: Option[String] = None,
  )(ps: Parser[T]*): Parser[T] = {
    val key = name match
      case Some(k) => k
      case None    => ps.map(_.toString).mkString(" | ")
    new Parser[T] {
      def apply(in: Input): ParseResult[T] = handleReader(in) { in =>
        val baseFailure: ParseResult[T] = Failure("Diverged Base", in)
        var kind = 0

        ps.foldLeft(baseFailure) {
          case (res, p) if res == baseFailure =>
            recordKind(p(in), key, kind)
          case (res, p) =>
            kind += 1
            res append recordKind(p(in), key, kind)
        }
      }
    }
  }

  /** record opt(`?`) */
  def recordOpt[T](p: => Parser[T]): Parser[T] = ???

  /** record longest(`|||`) */
  def recordLongest[T](ps: Parser[T]*): Parser[T] = ???

  /** parsers that records divergence */
  abstract class DivergedParser[+T <: Diverged] extends Parser[T]
  def record[T <: Diverged](p: => Parser[T]): DivergedParser[T] =
    new DivergedParser[T] {
      def apply(in: Input) = handleReader(in) { in =>
        (start ~> p ~ end ^^ { case o ~ m => o.setMap(m) })(in)
      }
    }
}
