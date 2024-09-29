package esmeta.analyzer

trait ViewLikeDecl { self: Analyzer =>

  /** view abstraction for analysis sensitivities */
  trait ViewLike {

    /** empty check */
    def isEmpty: Boolean
  }
}
