package esmeta.domain

import esmeta.error.*
import esmeta.util.*
import esmeta.util.Appender.{*, given}
import esmeta.util.BaseUtils.*

/** flat abstract domain */
trait FlatDomain[T] extends AbsDomain[T, Flat[T]] with Lattice[Flat[T]] {
  val Top = Flat.Many
  val Bot = Flat.Zero
  override def alpha(elems: Iterable[T]): Flat[T] = Flat(elems)
}

given [T]: Lattice.Ops[Flat[T]] with
  extension (x: Flat[T]) {
    def isTop: Boolean = x.isTop
    def isBottom: Boolean = x.isBottom
    def ⊑(y: Flat[T]): Boolean = x ⊑ y
    def ⊔(y: Flat[T]): Flat[T] = x ⊔ y
    def ⊓(y: Flat[T]): Flat[T] = x ⊓ y
  }

given [T]: AbsDomain.GenericOps[T, Flat[T]] with
  extension (x: Flat[T]) {
    def contains(value: T): Boolean = x.contains(value)
    def toBSet: BSet[T] = x.toBSet
    def toFlat: Flat[T] = x
  }
