package esmeta.ai.domain

import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*

/** domain */
trait Domain[A] {

  /** concrete element type * */
  type Concrete = A

  /** top element */
  def Top: Elem

  /** bottom element */
  def Bot: Elem

  /** element */
  type Elem

  /** abstraction functions */
  def alpha(elems: Iterable[A]): Elem
  def alpha(elems: A*): Elem = alpha(elems)
  def apply(elems: Iterable[A]): Elem = alpha(elems)
  def apply(elems: A*): Elem = alpha(elems)

  /** appender */
  given rule: Rule[Elem]

  /** optional domain */
  def optional(config: Config): Domain[Option[A]] = OptionDomain(config, this)

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
    def -(that: Elem): Elem = elem

    /** top check */
    def isTop: Boolean = elem == Top

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
