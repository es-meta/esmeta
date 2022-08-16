package esmeta.analyzer.domain

import esmeta.state.Bool

// Boolean domain
trait BoolDomain extends Domain {
  // top element
  val Top: Elem

  // abstraction functions
  def apply(elems: Bool*): Elem

  // boolean element interfaces
  extension (elem: Elem) {
    def unary_! : Elem
    def ||(that: Elem): Elem
    def &&(that: Elem): Elem
    def ⊓(that: Elem): Elem
    def -(that: Elem): Elem
    def getSingle: Flat[Bool]
  }
}
