package esmeta.analyzer.domain

import esmeta.util.Appender.*
import esmeta.analyzer.exploded

/** set domain */
trait SetDomain[A](
  val topName: String, // name of top element
  val maxSizeOpt: Option[Int] = None, // max size of set
  val totalOpt: BSet[A] = Inf, // total elements
) extends Domain[A] {

  /** elements */
  sealed trait Elem extends Iterable[A] with Appendable {

    /** iterators */
    final def iterator: Iterator[A] = (this match {
      case Base(set) => set
      case Top =>
        totalOpt match
          case Fin(set) => set
          case Inf =>
            exploded(s"impossible to concretize the top value of $topName.")
    }).iterator
  }

  /** top element */
  object Top extends Elem

  /** finite elements */
  case class Base(set: Set[A]) extends Elem

  /** bottom element */
  val Bot = Base(Set())

  /** abstraction functions */
  def alpha(elems: Iterable[A]): Elem =
    val set = elems.toSet
    maxSizeOpt match
      case Some(max) if set.size > max => Top
      case _                           => Base(set)

  /** appender */
  given rule: Rule[Elem] = (app, elem) =>
    elem match
      case Top => app >> topName
      case Base(set) =>
        app >> (set.size match {
          case 0 => "⊥"
          case 1 => set.head.toString
          case _ => set.toList.map(_.toString).sorted.mkString("{", ", ", "}")
        })

  /** element interfaces */
  extension (elem: Elem) {

    /** partial order */
    def ⊑(that: Elem): Boolean = (elem, that) match
      case (_, Top)                 => true
      case (Top, _)                 => false
      case (Base(lset), Base(rset)) => lset subsetOf rset

    /** join operator */
    def ⊔(that: Elem): Elem = (elem, that) match
      case (Top, _) | (_, Top)      => Top
      case (Base(lset), Base(rset)) => alpha(lset ++ rset)

    /** meet operator */
    override def ⊓(that: Elem): Elem = (elem, that) match
      case (Top, _)                 => that
      case (_, Top)                 => elem
      case (Base(lset), Base(rset)) => Base(lset intersect rset)

    /** prune operator */
    override def -(that: Elem): Elem = (elem, that) match
      case (Bot, _) | (_, Top) => Bot
      case (_, Bot)            => elem
      case (Top, _: Base)      => Top
      case (Base(l), Base(r))  => if (l subsetOf r) Bot else Base(l -- r)

    /** concretization function */
    override def gamma: BSet[A] = elem match
      case Top       => totalOpt
      case Base(set) => Fin(set)

    /** get single value */
    override def getSingle: Flat[A] = elem match
      case Base(set) =>
        set.size match
          case 0 => Zero
          case 1 => One(set.head)
          case _ => Many
      case _ => Many

    /** contains check */
    def contains(target: A): Boolean = elem match
      case Top       => true
      case Base(set) => set contains target
  }
}
