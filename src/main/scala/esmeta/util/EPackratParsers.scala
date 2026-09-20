package esmeta.util

import scala.util.parsing.combinator.*
import scala.util.parsing.input.{Reader, Position}
import scala.collection.mutable
import java.util.concurrent.atomic.AtomicInteger

/** an extensible packrat parsers */
trait EPackratParsers extends Parsers {
  protected trait DataType { def next: Data }
  protected type Data <: DataType
  protected def defaultData: Data

  // option for evaluation
  def eval: Boolean = false
  private val parseCount: AtomicInteger = new AtomicInteger
  def getParseCount: Int = parseCount.get
  private val cacheCount: AtomicInteger = new AtomicInteger
  def getCacheCount: Int = cacheCount.get

  class EPackratReader[+T <: Elem](underlying: Reader[T]) extends Reader[T] {
    outer =>
    val data: Data = defaultData
    val rev: List[Elem] = Nil
    private[EPackratParsers] val cache =
      mutable.HashMap.empty[(Parser[?], Position), MemoEntry[?]]
    private[EPackratParsers] def getFromCache[T2](
      p: Parser[T2],
    ): Option[MemoEntry[T2]] =
      cache.get((p, pos)).asInstanceOf[Option[MemoEntry[T2]]]
    private[EPackratParsers] def updateCacheAndGet[T2](
      p: Parser[T2],
      w: MemoEntry[T2],
    ): MemoEntry[T2] = { cache.put((p, pos), w); w }
    private[EPackratParsers] val recursionHeads
      : mutable.HashMap[Position, Head] = mutable.HashMap.empty
    private[EPackratParsers] var lrStack: List[LR] = Nil
    override def source: java.lang.CharSequence = underlying.source
    override def offset: Int = underlying.offset
    def first: T = underlying.first
    def rest: Reader[T] = copy(underlying.rest, data.next, first :: rev)
    def pos: EPos = EPos(underlying.pos, false)
    def atEnd: Boolean = underlying.atEnd
    def matchEmpty: Reader[T] = new EPackratReader(underlying) {
      override val data = outer.data
      override val rev = outer.rev
      override private[EPackratParsers] val cache = outer.cache
      override def pos: EPos = outer.pos.copy(emptyMatched = true)
      override private[EPackratParsers] val recursionHeads =
        outer.recursionHeads
      lrStack = outer.lrStack
    }
    def copy[T <: Elem](
      underlying: Reader[T] = this.underlying,
      newData: Data = this.data,
      newRev: List[Elem] = this.rev,
    ): EPackratReader[T] = new EPackratReader(underlying) {
      override val data = newData
      override val rev = newRev
      override private[EPackratParsers] val cache = outer.cache
      override private[EPackratParsers] val recursionHeads =
        outer.recursionHeads
      lrStack = outer.lrStack
    }
  }

  // extended position for left-recursion with empty string
  case class EPos(underlying: Position, emptyMatched: Boolean)
    extends Position {
    def line = underlying.line
    def column = underlying.column
    protected def lineContents =
      underlying.longString.substring(
        0,
        underlying.longString.length - column - 1,
      )
    override def <(that: Position): Boolean = {
      this.line < that.line ||
      this.line == that.line && this.column < that.column ||
      this.line == that.line && this.column == that.column &&
      that.isInstanceOf[EPos] && !this.emptyMatched && that
        .asInstanceOf[EPos]
        .emptyMatched
    }
  }

  // empty symbol
  object EMPTY extends EPackratParser[String] {
    def apply(in: Input): ParseResult[String] =
      Success("", in.asInstanceOf[EPackratReader[Elem]].matchEmpty)
  }

  // memoization for Packrat parsing
  def memo[T](p: super.Parser[T]): EPackratParser[T] = new EPackratParser[T] {
    def apply(in: Input) =
      val inMem = in.asInstanceOf[EPackratReader[Elem]]
      val m = recall(p, inMem)
      // count the number of parsing or using cached results
      if (eval) (if (m.isDefined) cacheCount else parseCount).incrementAndGet
      m match
        case None =>
          val base = LR(Failure("Base Failure", in), p, None)
          inMem.lrStack = base :: inMem.lrStack
          inMem.updateCacheAndGet(p, MemoEntry(Left(base)))
          val tempRes = p(in)
          inMem.lrStack = inMem.lrStack.tail
          base.head match
            case None =>
              inMem.updateCacheAndGet(p, MemoEntry(Right(tempRes)))
              tempRes
            case s @ Some(_) =>
              base.seed = tempRes
              val res = lrAnswer(p, inMem, base)
              res
        case Some(mEntry) =>
          mEntry match
            case MemoEntry(Left(recDetect)) =>
              setupLR(p, inMem, recDetect)
              recDetect match
                case LR(seed, _, _) => seed.asInstanceOf[ParseResult[T]]
            case MemoEntry(Right(res: ParseResult[_])) =>
              res.asInstanceOf[ParseResult[T]]
  }

  override def phrase[T](p: Parser[T]): EPackratParser[T] = {
    val q = super.phrase(p)
    new EPackratParser[T] {
      def apply(in: Input) = in match {
        case in: EPackratReader[_] => q(in)
        case in                    => q(new EPackratReader(in))
      }
    }
  }

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  private def getPosFromResult(r: ParseResult[?]): Position = r.next.pos
  private case class MemoEntry[+T](var r: Either[LR, ParseResult[?]]) {
    def getResult: ParseResult[T] = r match {
      case Left(LR(res, _, _)) => res.asInstanceOf[ParseResult[T]]
      case Right(res)          => res.asInstanceOf[ParseResult[T]]
    }
  }
  private case class LR(
    var seed: ParseResult[?],
    var rule: Parser[?],
    var head: Option[Head],
  ) { def getPos: Position = getPosFromResult(seed) }
  private case class Head(
    var headParser: Parser[?],
    var involvedSet: List[Parser[?]],
    var evalSet: List[Parser[?]],
  ) { def getHead = headParser }
  abstract class EPackratParser[+T] extends super.Parser[T]
  implicit def parser2packrat[T](p: => super.Parser[T]): EPackratParser[T] =
    lazy val q = p
    memo(super.Parser { in => q(in) })
  private def recall(
    p: super.Parser[?],
    in: EPackratReader[Elem],
  ): Option[MemoEntry[?]] = {
    val cached = in.getFromCache(p)
    val head = in.recursionHeads.get(in.pos)
    head match
      case None => cached
      case Some(h @ Head(hp, involved, evalSet)) =>
        if (cached.isEmpty && !(hp :: involved contains p))
          return Some(MemoEntry(Right(Failure("dummy ", in))))
        if (evalSet contains p)
          h.evalSet = h.evalSet.filterNot(_ == p)
          val tempRes = p(in)
          val tempEntry: MemoEntry[?] = cached.get
          tempEntry.r = Right(tempRes)
        cached
  }
  private def setupLR(
    p: Parser[?],
    in: EPackratReader[?],
    recDetect: LR,
  ): Unit =
    if (recDetect.head.isEmpty) recDetect.head = Some(Head(p, Nil, Nil))
    for (x <- in.lrStack.takeWhile(_.rule != p))
      x.head = recDetect.head
      recDetect.head.map(h => h.involvedSet = x.rule :: h.involvedSet)
  private def lrAnswer[T](
    p: Parser[T],
    in: EPackratReader[Elem],
    growable: LR,
  ): ParseResult[T] = growable match
    case LR(seed, rule, Some(head)) =>
      if (head.getHead != p) seed.asInstanceOf[ParseResult[T]]
      else
        in.updateCacheAndGet(
          p,
          MemoEntry(Right(seed.asInstanceOf[ParseResult[T]])),
        )
        seed match
          case f @ Failure(_, _) => f
          case e @ Error(_, _)   => e
          case s @ Success(_, _) => grow(p, in, head)
    case _ => throw Exception("lrAnswer with no head !!")
  private def grow[T](
    p: super.Parser[T],
    rest: EPackratReader[Elem],
    head: Head,
  ): ParseResult[T] =
    rest.recursionHeads.put(rest.pos, head)
    val oldRes: ParseResult[T] = rest.getFromCache(p).get match
      case MemoEntry(Right(x)) => x.asInstanceOf[ParseResult[T]]
      case _                   => throw Exception("impossible match")
    head.evalSet = head.involvedSet
    val tempRes = p(rest)
    tempRes match
      case s @ Success(_, _) =>
        if (getPosFromResult(oldRes) < getPosFromResult(tempRes))
          rest.updateCacheAndGet(p, MemoEntry(Right(s)))
          grow(p, rest, head)
        else
          rest.recursionHeads -= rest.pos
          rest.getFromCache(p).get match
            case MemoEntry(Right(x: ParseResult[_])) =>
              x.asInstanceOf[ParseResult[T]]
            case _ => throw Exception("impossible match")
      case f =>
        rest.recursionHeads -= rest.pos
        oldRes
}
