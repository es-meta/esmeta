package esmeta.ai.domain.bool

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.ai.domain.util.*
import esmeta.state.*

/** simple domain for boolean values */
object SimpleDomain
  extends bool.Domain
  with domain.SimpleDomain[Bool]("bool", Fin(T, F)) {

  // interfaces
  extension (elem: Elem) {

    /** unary negation */
    def unary_! : Elem = elem

    /** logical OR */
    def ||(that: Elem): Elem = (elem, that) match
      case (Top, Top) => Top
      case _          => Bot

    /** logical AND */
    def &&(that: Elem): Elem = (elem, that) match
      case (Top, Top) => Top
      case _          => Bot
  }
}
