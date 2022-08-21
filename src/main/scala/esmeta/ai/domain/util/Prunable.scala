package esmeta.ai.domain.util

import esmeta.ai.domain.*

/** domain */
trait Prunable[A] { this: Domain[A] =>

  /** domain element interfaces */
  extension (elem: Elem) {

    /** prune operator */
    def prune(that: Elem): Elem
  }
}
