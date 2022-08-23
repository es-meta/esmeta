package esmeta.ai.domain

/** prunable domain */
trait Prunable[A] { this: Domain[A] =>

  /** domain element interfaces */
  extension (elem: Elem) {

    /** prune operator */
    def -(that: Elem): Elem
  }
}
