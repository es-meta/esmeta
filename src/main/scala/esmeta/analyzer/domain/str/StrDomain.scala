package esmeta.analyzer.domain.str

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

trait StrDomainDecl { self: Self =>

  /** abstract string domain */
  trait StrDomain extends Domain[Str]
}
