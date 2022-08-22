package esmeta.ai.domain

import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*

/** domain */
trait Domain[A] {

  /** bottom element */
  val Bot: Elem

  /** element */
  type Elem

  /** abstraction functions */
  def alpha(elems: Iterable[A]): Elem
  def alpha(elems: A*): Elem = alpha(elems)
  def apply(elems: Iterable[A]): Elem = alpha(elems)
  def apply(elems: A*): Elem = alpha(elems)

  /** appender */
  given rule: Rule[Elem]

  /** domain element interfaces */
  extension (elem: Elem) {

    /** partial order */
    def ⊑(that: Elem): Boolean

    /** join operator */
    def ⊔(that: Elem): Elem

    /** not partial order */
    def !⊑(that: Elem): Boolean = !(elem ⊑ that)

    /** bottom check */
    def isBottom: Boolean = elem == Bot

    /** conversion to string */
    def toString: String = stringify(elem)

    /** concretization function */
    def gamma: BSet[A] = Inf

    /** get single value */
    def getSingle: Flat[A] = Many
  }
}
