package esmeta.domain

import esmeta.error.*
import esmeta.util.*
import esmeta.util.Appender.{*, given}
import esmeta.util.BaseUtils.*
import Lattice.{*, given}, Flat.*, BSet.*

/** abstract domain */
trait AbsDomain[C, A: AbsDomain.Ops[C]] extends Domain[A] { self =>

  /** concrete element type */
  type Conc = C

  /** abstraction */
  def alpha(elems: Iterable[C]): A

  /** abstraction */
  final inline def alpha(elems: C*): A = alpha(elems)

  /** abstraction */
  final inline def apply(elems: Iterable[C]): A = alpha(elems)

  /** abstraction */
  final inline def apply(elems: C*): A = alpha(elems)
}
object AbsDomain {
  type Ops[C] = [A] =>> GenericOps[C, A]
  trait GenericOps[C, A] {
    extension (x: A) {

      /** concretization to set domain */
      def contains(value: C): Boolean

      /** concretization to set domain */
      def toBSet: BSet[C]

      /** concretization to flat domain */
      def toFlat: Flat[C]
    }
  }
}
