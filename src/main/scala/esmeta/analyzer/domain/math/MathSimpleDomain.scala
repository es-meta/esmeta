package esmeta.analyzer.domain.math

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

trait MathSimpleDomainDecl { self: Self =>

  /** simple domain for mathematical values */
  object MathSimpleDomain extends MathDomain with SimpleDomain[Math]("math")
}
