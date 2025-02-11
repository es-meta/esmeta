package esmeta.util.domain

import esmeta.util.*
import esmeta.util.Appender.{*, given}
import esmeta.util.BaseUtils.*
import Flat.*, BSet.*

/** abstract domain */
trait AbsDomain extends Domain {

  /** concrete element type */
  type Conc

  /** abstract element type */
  type Elem

  /** abstraction */
  def alpha(elems: Iterable[Conc]): Elem

  /** abstraction */
  inline def alpha(elems: Conc*): Elem = alpha(elems)

  /** abstraction */
  inline def apply(elems: Iterable[Conc]): Elem = alpha(elems)

  /** abstraction */
  inline def apply(elems: Conc*): Elem = alpha(elems)

  extension (elem: Elem) {

    /** concretization to set domain */
    def toBSet: BSet[Conc]

    /** concretization to flat domain */
    def toFlat: Flat[Conc]
  }
}

/** set abstract domains */
trait BSetDomain[T] extends AbsDomain with Lattice {
  type Conc = T
  type Elem = BSet[Conc]
  val Top = Inf
  val Bot = Fin(Set())
  def alpha(elems: Iterable[T]): BSet[T] = BSet(elems)
  extension (elem: BSet[T]) {
    def isTop: Boolean = elem.isTop
    def isBottom: Boolean = elem.isBottom
    def ⊑(that: BSet[T]): Boolean = elem <= that
    def ⊔(that: BSet[T]): BSet[T] = elem || that
    def ⊓(that: BSet[T]): BSet[T] = elem && that
    def contains(value: T): Boolean = elem contains value
    def toBSet: BSet[T] = elem.toBSet
    def toFlat: Flat[T] = elem.toFlat
  }
}

/** flat abstract domain */
trait FlatDomain[T] extends AbsDomain with Lattice {
  type Conc = T
  type Elem = Flat[T]
  val Top = Many
  val Bot = Zero
  def alpha(elems: Iterable[T]): Flat[T] = Flat(elems)
  extension (elem: Flat[T]) {
    def isTop: Boolean = elem.isTop
    def isBottom: Boolean = elem.isBottom
    def ⊑(that: Flat[T]): Boolean = elem <= that
    def ⊔(that: Flat[T]): Flat[T] = elem || that
    def ⊓(that: Flat[T]): Flat[T] = elem && that
    def contains(value: T): Boolean = elem contains value
    def toBSet: BSet[T] = elem.toBSet
    def toFlat: Flat[T] = elem.toFlat
  }
}

/** simple abstract domains */
trait SimpleDomain[T] extends AbsDomain with Lattice {
  type Conc = T
  opaque type Elem = Boolean
  val Top = true
  val Bot = false
  def alpha(elems: Iterable[T]): Elem = elems.nonEmpty
  extension (elem: Elem) {
    def isTop: Boolean = elem
    def isBottom: Boolean = !elem
    def ⊑(that: Elem): Boolean = !elem || that
    def ⊔(that: Elem): Elem = elem || that
    def ⊓(that: Elem): Elem = elem && that
    def contains(value: T): Boolean = elem
    def toBSet: BSet[T] = if (elem) Inf else Fin(Set())
    def toFlat: Flat[T] = if (elem) Many else Zero
  }
}
