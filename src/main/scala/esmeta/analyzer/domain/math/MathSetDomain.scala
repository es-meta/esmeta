package esmeta.analyzer.domain.math

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

trait MathSetDomainDecl { self: Self =>

  /** set domain for mathematical values */
  class MathSetDomain(
    maxSizeOpt: Option[Int] = None, // max size of set
  ) extends MathDomain
    with SetDomain[Math]("math", maxSizeOpt)
}
