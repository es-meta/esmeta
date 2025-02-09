package esmeta.util.domain

import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*
import Flat.*, BSet.*

/** abstract domain */
trait Domain {

  /** concrete element type */
  type Conc

  /** abstract element type */
  type Elem

  /** top element */
  def Top: Elem

  /** bottom element */
  def Bot: Elem

  /** abstraction */
  def alpha(elems: Iterable[Conc]): Elem

  /** abstraction */
  inline def alpha(elems: Conc*): Elem = alpha(elems)

  /** abstraction */
  inline def apply(elems: Iterable[Conc]): Elem = alpha(elems)

  /** abstraction */
  inline def apply(elems: Conc*): Elem = alpha(elems)

  extension (elem: Elem) {

    /** top element check */
    def isTop: Boolean

    /** bottom element check */
    def isBottom: Boolean

    /** partial order */
    def ⊑(that: Elem): Boolean

    /** not partial order */
    inline def !⊑(that: Elem): Boolean = !(elem ⊑ that)

    /** join operator */
    def ⊔(that: Elem): Elem

    /** meet operator */
    def ⊓(that: Elem): Elem

    /** contains operator */
    def contains(value: Conc): Boolean

    /** concretization to set domain */
    def toBSet: BSet[Conc]

    /** concretization to flat domain */
    def toFlat: Flat[Conc]
  }
}
