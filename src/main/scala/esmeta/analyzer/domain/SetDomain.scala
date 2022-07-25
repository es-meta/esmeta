package esmeta.analyzer.domain

import esmeta.util.Appender
import esmeta.util.Appender.*
import esmeta.analyzer.exploded

/** set domain */
trait SetDomain[A] extends Domain {
  // name of top element
  protected val topName: String

  // max size of set
  protected val maxSizeOpt: Option[Int]

  // total elemetns
  protected val totalOpt: Option[Iterable[A]]

  // elements
  val Bot = Base(Set())
  object Top extends Elem
  case class Base(set: Set[A]) extends Elem

  // abstraction functions
  def apply(elems: A*): Elem = this(elems)
  def apply(elems: Iterable[A]): Elem = alpha(elems)
  def alpha(elems: Iterable[A]): Elem =
    val elemSet = elems.toSet
    maxSizeOpt match
      case Some(max) if elemSet.size > max => Top
      case _                               => Base(elemSet)

  // appender
  given rule: Rule[Elem] = (app, elem) =>
    elem match
      case Top => app >> topName
      case Base(set) =>
        app >> (set.size match {
          case 0 => "⊥"
          case 1 => set.head.toString
          case _ => set.toList.map(_.toString).sorted.mkString("{", ", ", "}")
        })

  // elements
  sealed trait Elem extends Iterable[A] with ElemTrait {
    // partial order
    def ⊑(that: Elem): Boolean = (this, that) match
      case (_, Top)                 => true
      case (Top, _)                 => false
      case (Base(lset), Base(rset)) => lset subsetOf rset

    // join operator
    def ⊔(that: Elem): Elem = (this, that) match
      case (Top, _) | (_, Top)      => Top
      case (Base(lset), Base(rset)) => alpha(lset ++ rset)

    // meet operator
    def ⊓(that: Elem): Elem = (this, that) match
      case (Top, _)                 => that
      case (_, Top)                 => this
      case (Base(lset), Base(rset)) => Base(lset intersect rset)

    // minus operator
    def -(that: Elem): Elem = (this, that) match
      case (Bot, _) | (_, Top) => Bot
      case (_, Bot)            => this
      case (Top, _: Base)      => Top
      case (Base(l), Base(r))  => if (l subsetOf r) Bot else Base(l -- r)

    // get single value
    def getSingle: Flat[A] = this match
      case Base(set) =>
        set.size match
          case 0 => FlatBot
          case 1 => FlatElem(set.head)
          case _ => FlatTop
      case _ => FlatTop

    // iterators
    final def iterator: Iterator[A] = (this match {
      case Base(set) => set
      case Top =>
        totalOpt.getOrElse {
          exploded(s"impossible to concretize the top value of $topName.")
        }
    }).iterator

    // contains check
    def contains(elem: A): Boolean = this match
      case Top       => true
      case Base(set) => set contains elem
  }
}
object SetDomain {
  // constructors
  def apply[A](
    topName: String,
    maxSizeOpt: Option[Int] = None,
    totalOpt: Option[Iterable[A]] = None,
  ): SetDomain[A] =
    val (n, m, t) = (topName, maxSizeOpt, totalOpt)
    new SetDomain[A] {
      protected val topName: String = n
      protected val maxSizeOpt: Option[Int] = m
      protected val totalOpt: Option[Iterable[A]] = t
    }
}
