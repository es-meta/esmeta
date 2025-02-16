package esmeta.domain

import esmeta.error.*
import esmeta.util.*
import esmeta.util.Appender.{*, given}
import esmeta.util.BaseUtils.*
import Lattice.{*, given}, Flat.*, BSet.*

/** set abstract domains */
trait SetDomain[T] extends AbsDomain[T, Set[T]] with Lattice[Set[T]] {
  type Conc = T
  type Elem = Set[Conc]
  lazy val Top = exploded("top element")
  val Bot = Set()
  override def alpha(elems: Iterable[T]): Set[T] = elems.toSet
}

given [T]: Lattice.Ops[Set[T]] with
  extension (x: Set[T]) {
    def isTop: Boolean = false
    def isBottom: Boolean = x.isEmpty
    def ⊑(y: Set[T]): Boolean = x.subsetOf(y)
    def ⊔(y: Set[T]): Set[T] = x.union(y)
    def ⊓(y: Set[T]): Set[T] = x.intersect(y)
  }

given [T]: AbsDomain.GenericOps[T, Set[T]] with
  extension (x: Set[T]) {
    def contains(value: T): Boolean = x.contains(value)
    def toBSet: BSet[T] = BSet(x)
    def toFlat: Flat[T] = Flat(x)
  }
