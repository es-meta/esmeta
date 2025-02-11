package esmeta.util.domain

import esmeta.util.BaseUtils.*

/** complete lattice */
trait Lattice extends Domain {

  /** lattice operations */
  given ops: Lattice.Ops[Elem]
}
object Lattice {
  trait DirectOps[Elem <: DirectOps[Elem]] { this: Elem =>
    def isTop: Boolean
    def isBottom: Boolean
    def ⊑(y: Elem): Boolean
    def ⊔(y: Elem): Elem
    def ⊓(y: Elem): Elem
  }
  trait DirectLattice extends Lattice {
    type Elem <: DirectOps[Elem]
    given ops: Ops[Elem] with
      extension (x: Elem) {
        def isTop: Boolean = x.isTop
        def isBottom: Boolean = x.isBottom
        def ⊑(y: Elem): Boolean = x ⊑ y
        def ⊔(y: Elem): Elem = x ⊔ y
        def ⊓(y: Elem): Elem = x ⊓ y
      }
  }
  trait Ops[Elem] {
    extension (x: Elem) {

      /** top element check */
      def isTop: Boolean
      final inline def nonTop: Boolean = !x.isTop

      /** bottom element check */
      def isBottom: Boolean
      final inline def nonBottom: Boolean = !x.isBottom

      /** partial order */
      def ⊑(y: Elem): Boolean
      final inline def <=(y: Elem): Boolean = x ⊑ y

      /** not partial order */
      final inline def !⊑(y: Elem): Boolean = !(x ⊑ y)
      final inline def !<=(y: Elem): Boolean = !(x ⊑ y)

      /** join operator */
      def ⊔(y: Elem): Elem
      final inline def ||(y: Elem): Elem = x ⊔ y

      /** meet operator */
      def ⊓(y: Elem): Elem
      final inline def &&(y: Elem): Elem = x ⊓ y
    }
  }

  given boolOps: Ops[Boolean] with
    extension (x: Boolean) {
      def isTop: Boolean = x
      def isBottom: Boolean = !x
      def ⊑(y: Boolean): Boolean = !x || y
      def ⊔(y: Boolean): Boolean = x || y
      def ⊓(y: Boolean): Boolean = x && y
    }

  given flatOps[T]: Ops[Flat[T]] with
    extension (x: Flat[T]) {
      def isTop: Boolean = x.isTop
      def isBottom: Boolean = x.isBottom
      def ⊑(y: Flat[T]): Boolean = x <= y
      def ⊔(y: Flat[T]): Flat[T] = x || y
      def ⊓(y: Flat[T]): Flat[T] = x && y
    }

  given bsetOps[T]: Ops[BSet[T]] with
    extension (x: BSet[T]) {
      def isTop: Boolean = x.isTop
      def isBottom: Boolean = x.isBottom
      def ⊑(y: BSet[T]): Boolean = x <= y
      def ⊔(y: BSet[T]): BSet[T] = x || y
      def ⊓(y: BSet[T]): BSet[T] = x && y
    }

  given setOps[T]: Ops[Set[T]] with
    extension (x: Set[T]) {
      def isTop: Boolean = exploded("top element")
      def isBottom: Boolean = x.isEmpty
      def ⊑(y: Set[T]): Boolean = x subsetOf y
      def ⊔(y: Set[T]): Set[T] = x ++ y
      def ⊓(y: Set[T]): Set[T] = x intersect y
    }

  given mapOps[K, V: Ops](using default: K => V): Ops[Map[K, V]] with
    extension (x: Map[K, V]) {
      def isTop: Boolean = false
      def isBottom: Boolean = x.isEmpty
      def ⊑(y: Map[K, V]): Boolean = x.forall {
        case (k, v) => y.get(k).fold(true)(v ⊑ _)
      }
      def ⊔(y: Map[K, V]): Map[K, V] = (for {
        k <- (x.keySet ++ y.keySet).toList
      } yield k -> (x.safeGet(k) ⊔ y.safeGet(k))).toMap
      def ⊓(y: Map[K, V]): Map[K, V] = (for {
        k <- (x.keySet ++ y.keySet).toList
      } yield k -> (x.safeGet(k) ⊓ y.safeGet(k))).toMap
      def safeGet(k: K): V = x.getOrElse(k, default(k))
    }
}
