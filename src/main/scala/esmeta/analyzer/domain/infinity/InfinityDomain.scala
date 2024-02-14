package esmeta.analyzer.domain.infinity

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.Infinity

trait InfinityDomainDecl { self: Self =>

  /** abstract boolean domain */
  trait InfinityDomain extends Domain[Infinity] {

    /** boolean element interfaces */
    extension (elem: Elem) {

      /** unary negation */
      def unary_! : Elem

      /** logical OR */
      def ||(that: Elem): Elem

      /** logical AND */
      def &&(that: Elem): Elem
    }
  }
}
