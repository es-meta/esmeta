package esmeta.domain

import esmeta.error.*
import esmeta.util.*
import esmeta.util.Appender.{*, given}
import esmeta.util.BaseUtils.*

/** simple abstract domains */
trait SimpleDomain[T] extends AbsDomain[T, Boolean] with Lattice[Boolean] {
  val Top = true
  val Bot = false
  override def alpha(elems: Iterable[T]): Boolean = elems.nonEmpty
}

given Lattice.Ops[Boolean] with
  extension (elem: Boolean) {
    def isTop: Boolean = elem
    def isBottom: Boolean = !elem
    def ⊑(that: Boolean): Boolean = !elem || that
    def ⊔(that: Boolean): Boolean = elem || that
    def ⊓(that: Boolean): Boolean = elem && that
  }

given [T]: AbsDomain.GenericOps[T, Boolean] with
  extension (elem: Boolean) {
    def contains(value: T): Boolean = elem
    def toBSet: BSet[T] = if (elem) BSet.Inf else BSet()
    def toFlat: Flat[T] = if (elem) Flat.Many else Flat.Zero
  }
