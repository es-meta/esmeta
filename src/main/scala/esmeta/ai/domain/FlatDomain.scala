package esmeta.ai.domain

import esmeta.ai.*
import esmeta.util.Appender
import esmeta.util.Appender.*

/** flat domain */
trait FlatDomain[A](
  val topName: String, // name of top element
  val totalOpt: BSet[A], // total elements
) extends Domain[A]
  with Prunable[A]
  with Meetable[A] {

  /** elements */
  sealed trait Elem extends Iterable[A] {

    /** iterators */
    final def iterator: Iterator[A] = (this match {
      case Bot        => Nil
      case Base(elem) => List(elem)
      case Top =>
        totalOpt match
          case Fin(set) => set
          case Inf =>
            exploded(s"impossible to concretize the top value of $topName.")
    }).iterator
  }

  /** top element */
  object Top extends Elem

  /** single elements */
  case class Base(elem: A) extends Elem

  /** bottom element */
  object Bot extends Elem

  /** abstraction functions */
  def alpha(elems: Iterable[A]): Elem =
    val set = elems.toSet
    set.size match
      case 0 => Bot
      case 1 => Base(set.head)
      case _ => Top

  /** appender */
  given rule: Rule[Elem] = (app, elem) =>
    elem match
      case Bot     => app >> "⊥"
      case Top     => app >> topName
      case Base(v) => app >> v.toString

  /** element interfaces */
  extension (elem: Elem) {

    /** partial order */
    def ⊑(that: Elem): Boolean = (elem, that) match
      case (Bot, _) | (_, Top) => true
      case (_, Bot) | (Top, _) => false
      case (Base(l), Base(r))  => l == r

    /** join operator */
    def ⊔(that: Elem): Elem = (elem, that) match
      case (Bot, _) | (_, Top) => that
      case (_, Bot) | (Top, _) => elem
      case (Base(l), Base(r))  => if (l == r) elem else Top

    /** meet operator */
    def ⊓(that: Elem): Elem = (elem, that) match
      case (Bot, _) | (_, Top) => elem
      case (_, Bot) | (Top, _) => that
      case (Base(l), Base(r))  => if (l == r) elem else Bot

    /** prune operator */
    def prune(that: Elem): Elem = (elem, that) match
      case (Bot, _) | (_, Top) => Bot
      case (_, Bot)            => elem
      case (Top, _: Base)      => Top
      case (Base(l), Base(r))  => if (l == r) Bot else elem

    /** contains check */
    def contains(target: A): Boolean = target match
      case Bot     => false
      case Top     => true
      case Base(x) => x == target

    /** concretization function */
    override def gamma: BSet[A] = elem match
      case Top        => totalOpt
      case Base(elem) => Fin(Set(elem))
      case Bot        => Fin(Set())

    /** get single value */
    override def getSingle: Flat[A] = elem match
      case Top        => Many
      case Base(elem) => One(elem)
      case Bot        => Zero
  }
}
object FlatDomain:
  def apply[A](
    topName: String,
    totalOpt: BSet[A] = Inf,
  ): Domain[A] = new Domain[A] with FlatDomain[A](topName, totalOpt)
  def apply[A](
    topName: String,
    iter: A*,
  ): Domain[A] = new Domain[A] with FlatDomain[A](topName, Fin(iter.toSet))
