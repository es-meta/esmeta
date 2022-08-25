package esmeta.ai.domain.ret

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.state.*

/** abstract return value domain */
trait Domain extends domain.Domain[(AValue, State)] {

  /** constructors */
  def apply(
    value: AbsValue = AbsValue.Bot,
    state: AbsState = AbsState.Bot,
  ): Elem

  /** extractors */
  def unapply(elem: Elem): Option[(AbsValue, AbsState)]

  /** return value element interfaces */
  extension (elem: Elem) {

    /** getters */
    def value: AbsValue
    def state: AbsState

    /** get string */
    def getString(detail: Boolean): String
  }
}
