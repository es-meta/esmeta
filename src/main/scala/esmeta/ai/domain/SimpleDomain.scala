package esmeta.ai.domain

import esmeta.ai.*
import esmeta.util.Appender.*

/** simple domain */
trait SimpleDomain[A](
  val topName: String, // name of top element
  val totalOpt: BSet[A] = Inf, // total elements
) extends Domain[A] {

  /** elements */
  sealed trait Elem extends Iterable[A] with Appendable {

    /** iterators */
    final def iterator: Iterator[A] = (this match {
      case Bot => Nil
      case Top =>
        totalOpt match
          case Fin(set) => set
          case Inf =>
            exploded(s"impossible to concretize the top value of $topName.")
    }).iterator
  }

  /** top element */
  object Top extends Elem

  /** bottom element */
  object Bot extends Elem

  /** abstraction functions */
  def alpha(elems: Iterable[A]): Elem = if (elems.isEmpty) Bot else Top

  /** appender */
  given rule: Rule[Elem] = (app, elem) =>
    elem match
      case Bot => app >> "⊥"
      case Top => app >> topName.toString

  /** element interfaces */
  extension (elem: Elem) {

    /** partial order */
    def ⊑(that: Elem): Boolean = (elem, that) match
      case (Bot, _) | (_, Top) => true
      case (_, Bot) | (Top, _) => false

    /** join operator */
    def ⊔(that: Elem): Elem = (elem, that) match
      case (Bot, _) | (_, Top) => that
      case (_, Bot) | (Top, _) => elem

    /** meet operator */
    override def ⊓(that: Elem): Elem = (elem, that) match
      case (Bot, _) | (_, Top) => elem
      case (_, Bot) | (Top, _) => that

    /** prune operator */
    override def -(that: Elem): Elem = that match
      case Bot => elem
      case Top => Bot

    /** concretization function */
    override def gamma: BSet[A] = elem match
      case Bot => Fin(Set())
      case Top => totalOpt

    /** get single value */
    override def getSingle: Flat[A] = elem match
      case Bot => Zero
      case Top =>
        totalOpt match
          case Fin(set) if set.size == 1 => One(set.head)
          case _                         => Many
  }
}
