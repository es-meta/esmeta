package esmeta.util.domain

import Flat.*, BSet.*

/** simple abstract domains */
trait SimpleDomain[A] extends Domain[A, Boolean]:
  val Top = true
  val Bot = false
  def alpha(elems: Iterable[A]): Boolean = elems.nonEmpty

given simpleOps[A]: Ops[A, Boolean] with
  extension (elem: Boolean)
    def isTop: Boolean = elem
    def isBottom: Boolean = !elem
    def ⊑(that: Boolean): Boolean = !elem || that
    def ⊔(that: Boolean): Boolean = elem || that
    def ⊓(that: Boolean): Boolean = elem && that
    def contains(value: A): Boolean = elem
    def gamma: BSet[A] = if (elem) Inf else Fin(Set())
    def getSingle: Flat[A] = if (elem) Many else Zero
