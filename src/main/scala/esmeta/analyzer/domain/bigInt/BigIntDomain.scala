package esmeta.analyzer.domain.bigInt

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

trait BigIntDomainDecl { self: Self =>

  /** abstract big integer domain */
  trait BigIntDomain extends Domain[BigInt]
}
