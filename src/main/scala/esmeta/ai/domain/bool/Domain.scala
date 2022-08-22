package esmeta.ai.domain.bool

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.state.Bool

/** abstract boolean domain */
trait Domain
  extends domain.Domain[Bool]
  with Prunable[Bool]
  with Meetable[Bool] {

  /** top element */
  val Top: Elem

  /** boolean element interfaces */
  extension (elem: Elem) {

    /** unary negation */
    def unary_! : Elem

    /** logical OR */
    def ||(that: Elem): Elem

    /** logical AND */
    def &&(that: Elem): Elem

    /** meet operator */
    def ⊓(that: Elem): Elem

    /** meet operator */
    def prune(that: Elem): Elem
  }
}
