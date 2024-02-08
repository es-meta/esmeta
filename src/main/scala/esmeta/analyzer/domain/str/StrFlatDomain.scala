package esmeta.analyzer.domain.str

import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.state.*

trait StrFlatDomainDecl { self: Self =>

  /** flat domain for string values */
  object StrFlatDomain extends StrDomain with FlatDomain[Str]("str")
}
