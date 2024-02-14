package esmeta.analyzer.domain.infinity

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*
import esmeta.util.*

trait InfinityFlatDomainDecl { self: Self =>

  /** flat domain for boolean values */
  object InfinityFlatDomain
    extends InfinityDomain
    with FlatDomain[Infinity](
      "bool",
      Fin(Set(Infinity(true), Infinity(false))),
    ) {

    /** interfaces */
    extension (elem: Elem) {

      /** unary negation */
      def unary_! : Elem = elem match
        case Bot               => Bot
        case Top               => Top
        case Base(Infinity(b)) => Base(Infinity(!b))

      /** logical OR */
      def ||(that: Elem): Elem = alpha(for {
        Infinity(l) <- elem
        Infinity(r) <- that
      } yield Infinity(l || r))

      /** logical AND */
      def &&(that: Elem): Elem = alpha(for {
        Infinity(l) <- elem
        Infinity(r) <- that
      } yield Infinity(l && r))
    }
  }
}
