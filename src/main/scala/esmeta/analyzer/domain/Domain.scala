package esmeta.analyzer.domain

import esmeta.analyzer.*
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*

/** domain */
trait Domain[A] { self =>

  /** top element */
  def Top: Elem

  /** bottom element */
  def Bot: Elem

  /** element */
  type Elem <: Appendable

  /** conversion to iterable object */
  given Conversion[Elem, Iterable[A]] = _.toIterable(true)

  /** abstraction functions */
  def alpha(elems: Iterable[A]): Elem
  def alpha(elems: A*): Elem = alpha(elems)
  def apply(elems: Iterable[A]): Elem = alpha(elems)
  def apply(elems: A*): Elem = alpha(elems)

  /** appender */
  given rule: Rule[Elem]

  /** appendable */
  trait Appendable { this: Elem =>

    /** conversion to string */
    override def toString: String = stringify(this)
  }

  /** optional domain */
  lazy val optional: OptionDomain[A, this.type] = OptionDomain(this)

  /** domain element interfaces */
  extension (elem: Elem) {

    /** partial order */
    def ⊑(that: Elem): Boolean

    /** not partial order */
    def !⊑(that: Elem): Boolean = !(elem ⊑ that)

    /** join operator */
    def ⊔(that: Elem): Elem

    /** meet operator */
    def ⊓(that: Elem): Elem = Top

    /** prune operator */
    def --(that: Elem): Elem = elem

    /** top check */
    def isTop: Boolean = elem == Top

    /** bottom check */
    def isBottom: Boolean = elem == Bot

    /** concretization function */
    def gamma: BSet[A] = if (isBottom) Fin() else Inf

    /** get single value */
    def getSingle: Flat[A] = if (isBottom) Zero else Many

    /** conversion to iterable */
    def toIterable(stop: Boolean): Iterable[A] = new Iterable[A]:
      final def iterator: Iterator[A] = elem.gamma match
        case Inf =>
          if (stop) exploded(s"impossible to iterate infinite values")
          else Nil.iterator
        case Fin(set) => set.iterator
  }
}
