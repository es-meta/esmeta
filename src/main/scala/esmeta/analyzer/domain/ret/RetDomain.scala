package esmeta.analyzer.domain.ret

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

trait RetDomainDecl { self: Self =>

  /** abstract return value domain */
  trait RetDomain extends Domain[(AValue, State)] {

    /** constructors */
    def apply(
      value: AbsValue = AbsValue.Bot,
      state: AbsState = AbsState.Bot,
    ): Elem

    /** extractors */
    def unapply(elem: Elem): (AbsValue, AbsState)

    /** return value element interfaces */
    extension (elem: Elem) {

      /** wrap completion records */
      def wrapCompletion: Elem

      /** getters */
      def value: AbsValue
      def state: AbsState

      /** get string */
      def getString(detail: Boolean): String
    }
  }
}
