package esmeta.domain

import esmeta.util.BaseUtils.*

/** complete lattice */
trait Lattice[A: Lattice.Ops] extends Domain[A] {

  /** abstract element type */
  type Elem = A
}
object Lattice {
  trait Ops[A] {
    extension (x: A) {

      /** top element check */
      def isTop: Boolean
      final inline def nonTop: Boolean = !x.isTop

      /** bottom element check */
      def isBottom: Boolean
      final inline def nonBottom: Boolean = !x.isBottom

      /** partial order */
      def ⊑(that: A): Boolean

      /** not partial order */
      final inline def !⊑(that: A): Boolean = !(x ⊑ that)

      /** join operator */
      def ⊔(that: A): A

      /** meet operator */
      def ⊓(that: A): A

      /** handling bottom propagation */
      def checkBottom[B: Lattice.Ops](target: B)(result: A)(using
        lattice: Lattice[A],
      ): A = if (target.isBottom) lattice.Bot else result

      /** handling bottom propagation */
      def checkBottom[B: Lattice.Ops](targets: Iterable[B])(result: A)(using
        lattice: Lattice[A],
      ): A = if (targets.exists(_.isBottom)) lattice.Bot else result

      /** handling bottom propagation */
      inline def checkBottom[B: Lattice.Ops](targets: B*)(result: A)(using
        lattice: Lattice[A],
      ): A = checkBottom(targets)(result)
    }
  }
}
