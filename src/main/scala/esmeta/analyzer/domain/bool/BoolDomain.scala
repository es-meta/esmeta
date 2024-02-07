package esmeta.analyzer.domain.bool

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.Bool

trait BoolDomainDecl { self: Self =>

  /** abstract boolean domain */
  trait BoolDomain extends Domain[Bool] {

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
