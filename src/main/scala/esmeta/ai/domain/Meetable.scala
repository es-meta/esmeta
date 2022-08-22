package esmeta.ai.domain

/** meetable domain */
trait Meetable[A] { this: Domain[A] =>

  /** domain element interfaces */
  extension (elem: Elem) {

    /** meet operator */
    def ⊓(that: Elem): Elem
  }
}
