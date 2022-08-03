package esmeta.analyzer.domain

import esmeta.util.Appender
import esmeta.util.Appender.*
import esmeta.analyzer.util.exploded

/** flat domain */
trait FlatDomain[A] extends Domain {
  // name of top element
  protected val topName: String

  // total elements
  protected val totalOpt: Option[Iterable[A]]

  // explosion for top
  protected val isExploded: Boolean = false

  // elements
  object Bot extends Elem
  object Top extends Elem
  case class Base(elem: A) extends Elem
  private def top: Top.type = {
    if (isExploded) exploded(s"Top value is not supported: $topName")
    Top
  }

  // abstraction functions
  def apply(elems: A*): Elem = this(elems)
  def apply(elems: Iterable[A]): Elem = elems.size match
    case 0 => Bot
    case 1 => Base(elems.head)
    case _ => top

  // appender
  given rule: Rule[Elem] = (app, elem) =>
    elem match
      case Bot     => app >> "⊥"
      case Top     => app >> topName
      case Base(v) => app >> v.toString

  // elements
  sealed trait Elem extends Iterable[A] with ElemTrait {
    // partial order
    def ⊑(that: Elem): Boolean = (this, that) match
      case (Bot, _) | (_, Top) => true
      case (_, Bot) | (Top, _) => false
      case (Base(l), Base(r))  => l == r

    // join operator
    def ⊔(that: Elem): Elem = (this, that) match
      case (Bot, _) | (_, Top) => that
      case (_, Bot) | (Top, _) => this
      case (Base(l), Base(r))  => if (l == r) this else top

    // meet operator
    def ⊓(that: Elem): Elem = (this, that) match
      case (Bot, _) | (_, Top) => this
      case (_, Bot) | (Top, _) => that
      case (Base(l), Base(r))  => if (l == r) this else Bot

    // minus operator
    def -(that: Elem): Elem = (this, that) match
      case (Bot, _) | (_, Top) => Bot
      case (_, Bot)            => this
      case (Top, _: Base)      => Top
      case (Base(l), Base(r))  => if (l == r) Bot else this

    // get single value
    def getSingle: Flat[A] = this match
      case Bot        => FlatBot
      case Top        => FlatTop
      case Base(elem) => FlatElem(elem)

    // iterators
    final def iterator: Iterator[A] = (this match {
      case Bot        => Nil
      case Base(elem) => List(elem)
      case Top =>
        totalOpt.getOrElse {
          exploded(s"impossible to concretize the top value of $topName.")
        }
    }).iterator

    // contains check
    def contains(elem: A): Boolean = this match
      case Bot     => false
      case Top     => true
      case Base(x) => x == elem
  }
}
object FlatDomain {
  // constructors
  def apply[A](
    topName: String,
    totalOpt: Option[Iterable[A]] = None,
    isExploded: Boolean = false,
  ): FlatDomain[A] =
    val (n, t, e) = (topName, totalOpt, isExploded)
    new FlatDomain[A] {
      protected val topName: String = n
      protected val totalOpt: Option[Iterable[A]] = t
      override protected val isExploded: Boolean = e
    }
}
