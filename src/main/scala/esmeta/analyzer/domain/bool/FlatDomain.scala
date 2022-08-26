package esmeta.analyzer.domain.bool

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

/** flat domain for boolean values */
object FlatDomain
  extends bool.Domain
  with domain.FlatDomain[Bool]("bool", Fin(T, F)) {

  /** interfaces */
  extension (elem: Elem) {

    /** unary negation */
    def unary_! : Elem = elem match
      case Bot           => Bot
      case Top           => Top
      case Base(Bool(b)) => Base(Bool(!b))

    /** logical OR */
    def ||(that: Elem): Elem = alpha(for {
      Bool(l) <- elem
      Bool(r) <- that
    } yield Bool(l || r))

    /** logical AND */
    def &&(that: Elem): Elem = alpha(for {
      Bool(l) <- elem
      Bool(r) <- that
    } yield Bool(l && r))
  }
}
