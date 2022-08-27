package esmeta.analyzer.domain.bool

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*
import esmeta.util.*

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
