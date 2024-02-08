package esmeta.analyzer.domain.absent

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*
import esmeta.util.*

trait AbsentSimpleDomainDecl { self: Self =>

  /** simple domain for absent values */
  object AbsentSimpleDomain
    extends AbsentDomain
    with SimpleDomain("absent", Fin(Absent))
}
