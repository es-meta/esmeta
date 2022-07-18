package esmeta.analyzer.domain

import esmeta.interp.Bool

object FlatBoolDomain extends FlatDomain[Bool] {
  val topName = "bool"
  val totalOpt = Some(Set(T, F))

  extension (elem: Elem) {
    def unary_! : Elem = elem match
      case Bot           => Bot
      case Top           => Top
      case Base(Bool(b)) => Base(Bool(!b))

    def ||(that: Elem): Elem = FlatBoolDomain(for {
      Bool(l) <- elem
      Bool(r) <- that
    } yield Bool(l || r))

    def &&(that: Elem): Elem = FlatBoolDomain(for {
      Bool(l) <- elem
      Bool(r) <- that
    } yield Bool(l && r))
  }
}
