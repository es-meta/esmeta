package esmeta.analyzer.domain.absent

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

trait AbsentDomainDecl { self: Self =>

  /** abstract absent domain */
  trait AbsentDomain extends Domain[Absent]
}
