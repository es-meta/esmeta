package esmeta.domain

import esmeta.error.*
import esmeta.util.*
import esmeta.util.Appender.{*, given}
import esmeta.util.BaseUtils.*
import Lattice.{*, given}, Flat.*, BSet.*

/** bounded set abstract domains */
trait BSetDomain[T] extends AbsDomain[T, BSet[T]] with Lattice[BSet[T]] {
  type Conc = T
  type Elem = BSet[Conc]
  val Top = Inf
  val Bot = Fin(Set())
  override def alpha(elems: Iterable[T]): BSet[T] = BSet(elems)
}

given [T]: Lattice.Ops[BSet[T]] with
  extension (x: BSet[T]) {
    def isTop: Boolean = x.isTop
    def isBottom: Boolean = x.isBottom
    def ⊑(y: BSet[T]): Boolean = x ⊑ y
    def ⊔(y: BSet[T]): BSet[T] = x ⊔ y
    def ⊓(y: BSet[T]): BSet[T] = x ⊓ y
  }

given [T]: AbsDomain.GenericOps[T, BSet[T]] with
  extension (x: BSet[T]) {
    def contains(value: T): Boolean = x.contains(value)
    def toBSet: BSet[T] = x
    def toFlat: Flat[T] = x.toFlat
  }
