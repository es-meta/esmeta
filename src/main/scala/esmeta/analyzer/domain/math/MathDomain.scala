package esmeta.analyzer.domain.math

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

trait MathDomainDecl { self: Self =>

  /** abstract mathematical value domain */
  trait MathDomain extends Domain[Math]
}
