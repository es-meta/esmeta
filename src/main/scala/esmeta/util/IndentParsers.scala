package esmeta.util

import scala.util.parsing.combinator.*
import scala.util.parsing.input.{Reader, Position}
import scala.collection.mutable

/** parsers for indentations defined with packrat parsing */
trait IndentParsers extends BasicParsers {

  /** readers for IndentParsers */
  class IndentReader[+T](underlying: Reader[T]) extends Reader[T] { outer =>
    private[IndentParsers] val indents: List[Int] = Nil
    private[IndentParsers] val cache =
      mutable.HashMap.empty[(Parser[_], Position), MemoEntry[_]]
    private[IndentParsers] def getFromCache[T2](
      p: Parser[T2],
    ): Option[MemoEntry[T2]] =
      cache.get((p, pos)).asInstanceOf[Option[MemoEntry[T2]]]
    private[IndentParsers] def updateCacheAndGet[T2](
      p: Parser[T2],
      w: MemoEntry[T2],
    ): MemoEntry[T2] = { cache.put((p, pos), w); w }
    private[IndentParsers] val recursionHeads: mutable.HashMap[Position, Head] =
      mutable.HashMap.empty
    private[IndentParsers] var lrStack: List[LR] = Nil
    override def source: java.lang.CharSequence = underlying.source
    override def offset: Int = underlying.offset
    def first: T = underlying.first
    def rest: Reader[T] = copy(underlying = underlying.rest)
    def pos: Position = underlying.pos
    def atEnd: Boolean = underlying.atEnd

    // push a new indentation
    private[IndentParsers] def push(indent: Int): IndentReader[T] =
      copy(indents = indent :: indents)

    // pop an indentation
    private[IndentParsers] def pop: Option[(Int, IndentReader[T])] =
      indents match {
        case Nil              => None
        case indent :: remain => Some(indent, copy(indents = remain))
      }

    // copy with a new underlying and a new indentation stack
    private[IndentParsers] def copy[T](
      underlying: Reader[T] = this.underlying,
      indents: List[Int] = this.indents,
    ): IndentReader[T] =
      val newIndents = indents
      new IndentReader(underlying) {
        override private[IndentParsers] val indents = newIndents
        override private[IndentParsers] val cache = outer.cache
        override private[IndentParsers] val recursionHeads =
          outer.recursionHeads
        lrStack = outer.lrStack
      }
  }

  /** treat only spaces as white spaces */
  override val whiteSpace = "( |\n *$)+".r

  /** next symbol */
  val next: Parser[Unit] = new Parser[Unit] {
    val name = "next"
    def apply(in: Input) = in match {
      case in: IndentReader[Char] =>
        newlineWithSpaces(in) match {
          case Success(expected, newIn: IndentReader[Char]) =>
            in.indents match {
              case indent :: _ if expected != indent =>
                Failure(s"expected $expected but $indent indentations", in)
              case _ =>
                Success((), newIn.copy(indents = in.indents))
            }
          case Success(_, newIn) => Failure("not an IndentReader", newIn)
          case fail: NoSuccess   => fail
        }
      case in => Failure("not an IndentReader", in)
    }
  }

  /** indent symbols */
  val indent: Parser[Unit] = new Parser[Unit] {
    val name = "indent"
    def apply(in: Input) = in match {
      case in: IndentReader[Char] =>
        (in.indents, newlineWithSpaces(in)) match {
          case (Nil, Success(indent, _)) => Success((), in.push(indent))
          case (prevIndent :: _, Success(indent, _)) =>
            if (prevIndent < indent) Success((), in.push(indent))
            else Failure(s"> $prevIndent expected but $indent indentations", in)
          case (_, fail: NoSuccess) => fail
        }
      case in => Failure("not an IndentReader", in)
    }
  }

  /** dedent symbols */
  val dedent: Parser[Unit] = new Parser[Unit] {
    val name = "dedent"
    def apply(in: Input) = in match {
      case in: IndentReader[Char] =>
        (in.pop, newlineWithSpaces(in)) match {
          case (None, _) => Failure("no indentation for dedent", in)
          case (Some((prevIndent, newIn)), Success(indent, _)) =>
            if (indent < prevIndent) Success((), newIn)
            else Failure(s"< $prevIndent expected but $indent indentations", in)
          case (_, fail: NoSuccess) => fail
        }
      case in => Failure("not an IndentReader", in)
    }
  }

  /** packrat parsers */
  abstract class PackratParser[+T] extends super.Parser[T]

  /** implicit conversion from parsers to packrat parsers */
  implicit def parser2packrat[T](p: => super.Parser[T]): PackratParser[T] = {
    lazy val q = p
    memo(super.Parser { in => q(in) })
  }

  /** new definition of the `phrase` helper */
  override def phrase[T](p: Parser[T]): PackratParser[T] = {
    val q = super.phrase(p)
    new PackratParser[T] {
      def apply(in: Input) = in match {
        case in: IndentReader[_] => q(in)
        case in                  => q(new IndentReader(in))
      }
    }
  }

  /** memoization of a parser using packrat parsing with a growing mechanism */
  def memo[T](p: super.Parser[T]): PackratParser[T] = {
    new PackratParser[T] {
      def apply(in: Input) = {
        val inMem = in.asInstanceOf[IndentReader[Elem]]
        val m = recall(p, inMem)
        m match {
          case None =>
            val base = LR(Failure("Base Failure", in), p, None)
            inMem.lrStack = base :: inMem.lrStack
            inMem.updateCacheAndGet(p, MemoEntry(Left(base)))
            val tempRes = p(in)
            inMem.lrStack = inMem.lrStack.tail
            base.head match {
              case None =>
                inMem.updateCacheAndGet(p, MemoEntry(Right(tempRes)))
                tempRes
              case s @ Some(_) =>
                base.seed = tempRes
                val res = lrAnswer(p, inMem, base)
                res
            }

          case Some(mEntry) => {
            mEntry match {
              case MemoEntry(Left(recDetect)) => {
                setupLR(p, inMem, recDetect)
                recDetect match {
                  case LR(seed, _, _) => seed.asInstanceOf[ParseResult[T]]
                }
              }
              case MemoEntry(Right(res: ParseResult[_])) =>
                res.asInstanceOf[ParseResult[T]]
            }
          }
        }
      }
    }
  }

  // ----------------------------------------
  // private helpers
  // ----------------------------------------
  private val newlineWithSpaces: PackratParser[Int] =
    "\n *".r ^^ { _.length - 1 } | "$".r ^^^ 0

  private def getPosFromResult(r: ParseResult[_]): Position = r.next.pos

  private case class MemoEntry[+T](var r: Either[LR, ParseResult[_]]) {
    def getResult: ParseResult[T] =
      r match {
        case Left(LR(res, _, _)) => res.asInstanceOf[ParseResult[T]]
        case Right(res)          => res.asInstanceOf[ParseResult[T]]
      }
  }

  private case class LR(
    var seed: ParseResult[_],
    var rule: Parser[_],
    var head: Option[Head],
  ) { def getPos: Position = getPosFromResult(seed) }

  private case class Head(
    var headParser: Parser[_],
    var involvedSet: List[Parser[_]],
    var evalSet: List[Parser[_]],
  ) { def getHead = headParser }

  private def recall(
    p: super.Parser[_],
    in: IndentReader[Elem],
  ): Option[MemoEntry[_]] = {
    val cached = in.getFromCache(p)
    val head = in.recursionHeads.get(in.pos)
    head match {
      case None => cached
      case Some(h @ Head(hp, involved, evalSet)) => {
        if (cached.isEmpty && !(hp :: involved contains p)) {
          return Some(MemoEntry(Right(Failure("dummy ", in))))
        }
        if (evalSet contains p) {
          h.evalSet = h.evalSet.filterNot(_ == p)
          val tempRes = p(in)
          val tempEntry: MemoEntry[_] = cached.get
          tempEntry.r = Right(tempRes)
        }
        cached
      }
    }
  }

  private def setupLR(
    p: Parser[_],
    in: IndentReader[_],
    recDetect: LR,
  ): Unit = {
    if (recDetect.head.isEmpty) recDetect.head = Some(Head(p, Nil, Nil))

    in.lrStack.takeWhile(_.rule != p).foreach { x =>
      x.head = recDetect.head
      recDetect.head.map(h => h.involvedSet = x.rule :: h.involvedSet)
    }
  }

  private def lrAnswer[T](
    p: Parser[T],
    in: IndentReader[Elem],
    growable: LR,
  ): ParseResult[T] =
    growable match {
      case LR(seed, rule, Some(head)) =>
        if (head.getHead != p) seed.asInstanceOf[ParseResult[T]]
        else {
          in.updateCacheAndGet(
            p,
            MemoEntry(Right(seed.asInstanceOf[ParseResult[T]])),
          )
          seed match {
            case f @ Failure(_, _) => f
            case e @ Error(_, _)   => e
            case s @ Success(_, _) => grow(p, in, head)
          }
        }
      case _ => throw new Exception("lrAnswer with no head !!")
    }

  private def grow[T](
    p: super.Parser[T],
    rest: IndentReader[Elem],
    head: Head,
  ): ParseResult[T] = {
    rest.recursionHeads.put(rest.pos, head)
    val oldRes: ParseResult[T] = rest.getFromCache(p).get match {
      case MemoEntry(Right(x)) => x.asInstanceOf[ParseResult[T]]
      case _                   => throw new Exception("impossible match")
    }
    head.evalSet = head.involvedSet
    val tempRes = p(rest);
    tempRes match {
      case s @ Success(_, _) =>
        if (getPosFromResult(oldRes) < getPosFromResult(tempRes)) {
          rest.updateCacheAndGet(p, MemoEntry(Right(s)))
          grow(p, rest, head)
        } else {
          rest.recursionHeads -= rest.pos
          rest.getFromCache(p).get match {
            case MemoEntry(Right(x: ParseResult[_])) =>
              x.asInstanceOf[ParseResult[T]]
            case _ => throw new Exception("impossible match")
          }
        }
      case f =>
        rest.recursionHeads -= rest.pos
        oldRes
    }
  }
}
