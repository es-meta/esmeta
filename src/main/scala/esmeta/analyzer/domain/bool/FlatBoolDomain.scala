package esmeta.analyzer.domain

import esmeta.interp.Bool

/** flat domain for boolean */
object FlatBoolDomain extends FlatDomain[Bool] with BoolDomain {
  val topName = "bool"
  val totalOpt = Some(Set(T, F))

  // interfaces
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

    def ⊓(that: Elem): Elem = elem ⊓ that
    def getSingle: Flat[Bool] = elem.getSingle
  }
}
