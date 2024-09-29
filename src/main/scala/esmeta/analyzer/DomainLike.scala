package esmeta.analyzer

trait DomainLikeDecl { self: Analyzer =>

  /** abstract states */
  trait AbsStateLike {

    /** has imprecise elements */
    def hasImprec: Boolean
  }

  /** abstract return values */
  trait AbsRetLike {

    /** return value */
    def value: AbsValue
  }

  /** abstract values */
  trait AbsValueLike {

    /** get string of abstract value with an abstract state */
    def getString(state: AbsState): String
  }
}
