package esmeta.util.domain

import esmeta.error.*
import esmeta.util.*
import esmeta.util.Appender.{*, given}
import esmeta.util.BaseUtils.*
import Lattice.{*, given}, Flat.*, BSet.*

/** abstract domain */
trait AbsDomain extends Domain {

  /** concrete element type */
  type Conc

  /** abstraction */
  def alpha(elems: Iterable[Conc]): Elem

  /** abstraction */
  final inline def alpha(elems: Conc*): Elem = alpha(elems)

  /** abstraction */
  final inline def apply(elems: Iterable[Conc]): Elem = alpha(elems)

  /** abstraction */
  final inline def apply(elems: Conc*): Elem = alpha(elems)

  extension (elem: Elem) {

    /** concretization to set domain */
    def toBSet: BSet[Conc] = Inf

    /** concretization to flat domain */
    def toFlat: Flat[Conc] = Many
  }
}

/** simple abstract domains */
trait SimpleDomain[T] extends AbsDomain with Lattice {
  type Conc = T
  opaque type Elem = Boolean
  val Top = true
  val Bot = false
  override def alpha(elems: Iterable[T]): Elem = elems.nonEmpty
  given ops: Ops[Elem] = boolOps
  extension (elem: Elem) {
    def contains(value: T): Boolean = elem
    override def toBSet: BSet[T] = if (elem) Inf else Fin(Set())
    override def toFlat: Flat[T] = if (elem) Many else Zero
  }
}

/** flat abstract domain */
trait FlatDomain[T] extends AbsDomain with Lattice {
  type Conc = T
  type Elem = Flat[T]
  val Top = Many
  val Bot = Zero
  override def alpha(elems: Iterable[T]): Flat[T] = Flat(elems)
  given ops: Ops[Flat[T]] = flatOps
  extension (elem: Flat[T]) {
    def contains(value: T): Boolean = elem contains value
    override def toBSet: BSet[T] = elem.toBSet
    override def toFlat: Flat[T] = elem.toFlat
  }
}

/** set abstract domains */
trait SetDomain[T] extends AbsDomain with Lattice {
  type Conc = T
  type Elem = Set[Conc]
  lazy val Top = exploded("top element")
  val Bot = Set()
  override def alpha(elems: Iterable[T]): Set[T] = elems.toSet
  given ops: Ops[Elem] = setOps
  extension (elem: Set[T]) {
    def contains(value: T): Boolean = elem contains value
    override def toBSet: BSet[T] = BSet(elem)
    override def toFlat: Flat[T] =
      if (elem.isEmpty) Zero
      else if (elem.size == 1) One(elem.head)
      else Many
  }
}

/** bounded set abstract domains */
trait BSetDomain[T] extends AbsDomain with Lattice {
  type Conc = T
  type Elem = BSet[Conc]
  val Top = Inf
  val Bot = Fin(Set())
  override def alpha(elems: Iterable[T]): BSet[T] = BSet(elems)
  given ops: Ops[Elem] = bsetOps
  extension (elem: BSet[T]) {
    def contains(value: T): Boolean = elem contains value
    override def toBSet: BSet[T] = elem.toBSet
    override def toFlat: Flat[T] = elem.toFlat
  }
}
