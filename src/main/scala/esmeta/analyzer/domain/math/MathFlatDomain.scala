package esmeta.analyzer.domain.math

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

trait MathFlatDomainDecl { self: Self =>

  /** flat domain for mathematical values */
  object MathFlatDomain extends MathDomain with FlatDomain[Math]("math")
}
